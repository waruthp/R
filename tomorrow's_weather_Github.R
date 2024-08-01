# Load packages
library(rgee)
library(terra)
library(sf)
library(giscoR)
library(fs)
library(tidyverse)
library(lubridate)
library(ggshadow)
library(ggforce)

install.packages("future")

library(future)

ee_check() # check if everything is correct
#ee_Authenticate()
ee_Initialize(drive = TRUE)


# Access the Global Forecast System
# GFS forecast
dataset <- ee$ImageCollection('NOAA/GFS0P25')$filter(ee$Filter$date(rdate_to_eedate(today()-days(1)),
                                                                    rdate_to_eedate(today()+days(1))))
dataset

# Vector of unique run dates
last_run <- ee_get_date_ic(dataset)$time_start |> unique()
last_run

# Select the last one / 6 hrs
last_run <- max(last_run)

# Last run and variable selection
temp <- dataset$filter(ee$Filter$date(rdate_to_eedate(last_run)))$select('temperature_2m_above_ground')

# Define the forecast dates for each hour
forecast_time <- temp$map(ee_utils_pyfunc(function(img)  {
  return(ee$Image(img)$set('system:time_start',ee$Image(img)$get("forecast_time")))
}))

# Get the forecast dates
date_forecast <- ee_get_date_ic(forecast_time)
head(date_forecast)

# Define start and end of the period
endDate <- rdate_to_eedate(round_date(max(date_forecast$time_start)-days(1), "day"))
startDate <- rdate_to_eedate(round_date(min(date_forecast$time_start), "day"))

# Number of days
numberOfDays <- endDate$difference(startDate, 'days')

# Calculate the daily maximum
daily <- ee$ImageCollection(
  ee$List$sequence(0, numberOfDays$subtract(1))$
    map(ee_utils_pyfunc(function (dayOffset) {
      start = startDate$advance(dayOffset, 'days')
      end = start$advance(1, 'days')
      return(forecast_time$
               filterDate(start, end)$
               max()$ # alternative: min(), mean()
               set('system:time_start', start$millis()))
    }))
)

# Dates of the daily maximum
head(ee_get_date_ic(daily))

# Dynamic map via GEE
Map$addLayer(
  eeObject = daily$first(),
  visParams = list(min = -45, max = 45,
                   palette = rev(RColorBrewer::brewer.pal(11, "RdBu"))),
  name = "GFS") + 
  Map$addLegend(
    list(min = -45, max = 45, 
         palette = rev(RColorBrewer::brewer.pal(11, "RdBu"))), 
    name = "Maximum temperature", 
    position = "bottomright", 
    bins = 10)

# Export multiple images
# Earth extension
geom <- ee$Geometry$Polygon(coords = list(
  c(-180, -90), 
  c(180, -90),
  c(180, 90),
  c(-180, 90),
  c(-180, -90)
),
proj = "EPSG:4326",
geodesic = FALSE)

geom # EarthEngine object of type geometry

# Temporary download folder
tmp <- tempdir()

# Run tasks and download each day
ic_drive_files_2 <- ee_imagecollection_to_local(
  ic = daily$filter(ee$Filter$date(rdate_to_eedate(today()), rdate_to_eedate(today()+days(2)))), # we choose only the next 2 days
  region = geom,
  scale = 20000, # resolution 
  lazy = FALSE,
  dsn = file.path(tmp, "rast_"), # name of each raster
  add_metadata = TRUE
)

# Orthographic map
# Paths to downloaded data
forecast_world <- dir_ls(tmp, regexp = "tif")

# Check the extents of the raster files
extents <- lapply(forecast_world, function(file) {
  ext(rast(file))
})

# Print the extents
print(extents)

# Define a common extent and resolution
common_extent <- ext(rast(forecast_world[1]))
common_res <- res(rast(forecast_world[1]))

# Resample the raster files to the common extent and resolution
resampled_rasters <- lapply(forecast_world, function(file) {
  rast(file) %>%
    extend(common_extent) %>%
    resample(rast(file), method = "bilinear")
})

# Import the resampled raster files
forecast_rast <- rast(resampled_rasters)

# Print the imported raster object
print(forecast_rast)

# define the temporal dimension as the name of each layer
names(forecast_rast) <- seq(today(), today() + days(1), "day")
forecast_rast

# plot
plot(forecast_rast)


# projection definition
ortho_crs <-'+proj=ortho +lat_0=51 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# reproject the raster
ras_ortho <- project(forecast_rast, ortho_crs)

# convert the raster to a data.frame of xyz
forecast_df <- as.data.frame(ras_ortho, xy = TRUE)

# transform to a long format
forecast_df <- pivot_longer(forecast_df, 3:length(forecast_df), names_to = "date", values_to = "ta")

# Administrative boundaries and graticules
# obtain the administrative limits
world_poly <- gisco_get_countries(year = "2020", epsg = "4326", resolution = "10") 

# get the global graticule
grid <- st_graticule()

# define what would be ocean
ocean <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # earth radius
  st_sfc(crs = ortho_crs)
plot(ocean)

# select only visible from the boundaries and reproject
world <- world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 
plot(world)

# eliminate the lines that pass over the continents
grid_crp <- st_difference(grid, st_union(world_poly))

# select the visible part
grid_crp <- st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)

plot(grid_crp)

# convert the boundary of the globe into a data.frame
ocean_df <- st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# Map construction
forecast_tomorrow <- filter(forecast_df, date == today() + days(1)) |>
  mutate(ta_limit = case_when(ta > 45 ~ 45,
                              ta < -45 ~ -45,
                              TRUE ~ ta))

# Adding shadow of the globe
# build a simple shadow
ggplot() + 
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 2.0) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = 1.5) +
  coord_sf() +
  theme_void()

# combining several layers of shadow
g <- ggplot() +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.8) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5) 

# Adding other layers
g2 <- g + geom_raster(data = forecast_tomorrow, aes(x, y, fill = ta_limit)) +
  geom_sf(data = grid_crp, 
          colour = "white", 
          linewidth = .2) +
  geom_sf(data = world, 
          fill = NA,
          colour = "grey10",
          linewidth = .2) 

g2 + scale_fill_distiller(palette = "RdBu", 
                          limits = c(-25, 45),
                          breaks = c(-25, 0, 25, 45)) +
  guides(fill = guide_colourbar(barwidth = 15, 
                                barheight = .5, 
                                title.position = "top",
                                title.hjust = .5)) +
  coord_sf() +
  labs(fill = str_wrap("Maximum Temperature at 2 meters for August 2, 2024", 35)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)) 

labeling <- slice(forecast_tomorrow, which.min(ta), which.max(ta))
labeling

g2 +  geom_mark_circle(data = labeling, 
                       aes(x, y, 
                           description = str_glue('{scales::number(ta, accuracy = .1, decimal.mark = ".", style_positive = "plus", suffix = "ºC")}')
                       ), 
                       expand = unit(1, "mm"), 
                       label.buffer = unit(4, "mm"),
                       label.margin = margin(1, 1, 1, 1, "mm"),
                       con.size = 0.5,
                       con.colour = "blue",
                       colour = "blue",
                       label.fontsize = 10,
                       label.colour = "white",
                       label.fontface = "bold",
                       con.type = "straight",
                       label.fill = alpha("blue", .5)) +
  scale_fill_distiller(palette = "RdBu", 
                       limits = c(-25, 45),
                       breaks = c(-25, 0, 25, 45)) +
  guides(fill = guide_colourbar(barwidth = 15, 
                                barheight = .5, 
                                title.position = "top",
                                title.hjust = .5)) +
  coord_sf(crs = ortho_crs) +
  labs(fill = str_wrap("Maximum Temperature at 2 meters for August 2, 2024", 35)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)) 


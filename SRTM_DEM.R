webshot::install_phantomjs()

library(rgee)
library(mapview)
library(webshot)

# Initialize Earth Engine
ee_Initialize(drive = TRUE, user = "xxxxxxxx") # input your gmail.com

# Define the feature collection and filter for Japan
countries <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')
Japan <- countries$filter(ee$Filter$eq("country_na", "Japan")) # filter country name, in this example "Japan"

# Define the DEM image
dem <- ee$Image('USGS/SRTMGL1_003')

# Define visualization parameters
vis_01 <- list(
  min = 0,
  max = 4000,
  palette = c('006633', 'EFFFCC', '662A00', 'D8D8D8', 'F5F5F5')
)

# Center the map on Japan and add the DEM layer
Map$centerObject(Japan, 5)
map <- Map$addLayer(dem$clip(Japan)$updateMask(dem$gt(0)), visParams = vis_01, name = 'DEM')
map

# Save the map as a PNG file using mapshot
mapshot(map, file = "Japan_map.png")

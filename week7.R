library(here)
library(janitor)
library(sf)
library(tidyverse)
library(tmap)
#read the ward data in
LondonWards <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Ward.shp")
LondonWardsMerged <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")%>%
  st_transform(.,27700)
WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)
#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)
tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")
summary(BluePlaques)
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")



















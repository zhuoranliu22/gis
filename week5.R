library(sf)

Londonborough <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)

OSM <- st_read("/Users/apple/Desktop/CASA0005/week5/greater-london-latest-free.shp/gis_osm_pois_a_free_1.shp") %>%
  st_transform(., 27700) %>%
  #select hotels only
  dplyr::filter(fclass == 'hotel')

join_example <-  st_join(Londonborough, OSM)

head(join_example)

##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)

# read in all the spatial data and 
# reproject it 

OSM <- st_read("/Users/apple/Desktop/CASA0005/week5/greater-london-latest-free.shp/gis_osm_pois_a_free_1.shp") %>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')
Worldcities <- st_read("/Users/apple/Desktop/CASA0005/week5/World_Cities/World_Cities.shp") %>%
  st_transform(., 27700)

UK_outline <- st_read("/Users/apple/Desktop/CASA0005/week5/gadm41_GBR_shp/gadm41_GBR_0.shp") %>%
  st_transform(., 27700)
Londonborough <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)
Airbnb <- read_csv("/Users/apple/Desktop/CASA0005/week5/listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')
# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(data2,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')

newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)

UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(0, 5, 12, 26, 57, 286) 

# plot each map
tm1 <- tm_shape(Hotels) + 
  tm_polygons("Accomodation count", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm2 <- tm_shape(Airbnb) + 
  tm_polygons("Accomodation count",
              breaks=breaks, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

tm3 <- tm_shape(UK_outlinecrop)+ 
  tm_polygons(col="darkslategray1")+
  tm_layout(frame=FALSE)+
  tm_shape(Worldcities2) +
  tm_symbols(col = "red", scale = .5)+
  tm_text("CITY_NAME", xmod=-1, ymod=-0.5)

legend <- tm_shape(Hotels) +
  tm_polygons("Accomodation count",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))

t=tmap_arrange(tm1, tm2, tm3, legend, ncol=2)

t




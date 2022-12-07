#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

##First, get the London Borough Boundaries
LondonBoroughs <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

#BoroughMap <- BoroughMap %>%
#  st_transform(.,4326)
print(BoroughMap)
summary(BoroughMap)
#Now get the location of all Blue Plaques in the City
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson") %>%
  st_transform(.,27700)
summary(BluePlaques)
#plot the blue plaques in the city
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")
#remove duplicates
library(tidyverse)

library(sf)
BluePlaques <- distinct(BluePlaques)
print(BluePlaques)

BluePlaquesSub <- BluePlaques[BoroughMap,]
#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# add sparse=false to get the complete matrix.
intersect_indices <-st_intersects(BoroughMap, BluePlaques)
OSM <- st_read("/Users/apple/Desktop/CASA0005/week5/greater-london-latest-free.shp/gis_osm_pois_a_free_1.shp") %>%
  st_transform(., 27700) %>%
  #select hotels only
  dplyr::filter(fclass == 'hotel')
Londonborough <- st_read("/Users/apple/Desktop/CASA0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>%
  st_transform(.,27700) 
  

join_example <-  st_join(OSM, Londonborough)

nrow(join_example)

# read in the .csv
# and make it into spatial data

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
    st_join(Londonborough,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

all_accomodation <- st_join(Hotels, Airbnb)

head(all_accomodation)
all_accomodation <- st_join(Hotels, Airbnb, join = st_equals)

head(all_accomodation)
#extract the borough

# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)
#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")
BluePlaquesSub.ppp %>%
  density(., sigma=500) %>%
  plot()
BluePlaquesSub.ppp %>%
  density(., sigma=1000) %>%
  plot()
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")
#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)
Qcount %>% 
  summarise_all(class)
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 
lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")

K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()
Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "Ripley"))
library(raster)
library(fpc)
#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()
#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)
#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)
# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)
library(ggplot2)
db
db$cluster
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)
chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)
chulls <- chulls %>%
  filter(dbcluster >=1)
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()
###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()





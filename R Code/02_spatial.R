library(mapview)
library(ggplot2)
library(dplyr)
library(sp)

#data location sabinas
data <-read.csv("coordenadas.csv", sep=",")


xy <- data[,c("x", "y")]
spdata <- SpatialPointsDataFrame(coords = xy, data = data,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#map  
mapview(spdata, zcol = "area", legend = TRUE) # Africa

#turn coordinates
colnames(data) <- c("area", "ind", "y", "x")

# repeat
xy <- data[,c("x", "y")]
spdata <- SpatialPointsDataFrame(coords = xy, data = data,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#map  
mapview(spdata, zcol = "area", legend = TRUE) # Doñana!


#data camtraps
#data camtraps
datacam<-read.csv(file = "raw_data/JPHO_phototraping_definitive.csv", skip=18, sep= ";")


# number detections per ind
numdet<- datacam %>% 
        group_by(ind) %>%
        summarize(freq = n())

#join with spatial data
data_num <- inner_join(data, numdet, by = "ind") 


xy <- data_num[,c("x", "y")]
spdata_num <- SpatialPointsDataFrame(coords = xy, data = data_num,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#map  
mapview(spdata_num, zcol = "freq", legend = TRUE) # needs to be corrected by sampling effort


# # number detections per ind, per species
numdetsp<- datacam %>% 
  group_by(ind, sp) %>%
  summarize(freq = n())

#join with spatial data
data_numsp <- inner_join(data, numdetsp, by = "ind") 


xy <- data_numsp[,c("x", "y")]
spdata_numsp <- SpatialPointsDataFrame(coords = xy, data = data_numsp,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



#map vulpes
vul_vul<-spdata_numsp[spdata_numsp$sp == "vul_vul", ]
her_icn<-spdata_numsp[spdata_numsp$sp == "her_icn", ]

mapview(vul_vul, zcol = "freq", legend = TRUE) 
mapview(her_icn, zcol = "freq", legend = TRUE)  # needs to be corrected by sampling effort


mapview(vul_vul, col.regions = "red") +
mapview(her_icn, col.regions = "blue")  # needs to be corrected by sampling effort

?mapview



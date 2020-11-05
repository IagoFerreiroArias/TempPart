# SCRIPT IAGO ANALISIS ESPACIAL
library(dplyr)
library(ggplot2)
library(tmap)
library(mapview)
library(sf)
library(ggmap)

# import station data

coor_data <- read.csv("coordenadas.csv", skip =14, header=T, sep= ";")
coor_data<- rename(coor_data, ind = tree)
ggplot(coor_data, aes(x= coor_data$x , y=coor_data$y)) + geom_point()


# extract detections of each sp per station
meso_data <- read.csv("meso_data.csv", header = T, sep=",")

genetta <- subset(meso_data, binomial == "Genetta genetta")
vulpes <- subset(meso_data, binomial =="Vulpes vulpes")
meles <- subset(meso_data, binomial =="Meles meles")
herpestes <- subset(meso_data, binomial == "Herpestes ichneumon")

rabbit <- subset(meso_data, binomial == "Oryctolagus cuniculus")
rodent <- subset(meso_data, sp=="rodent")
partridge <- subset(meso_data, binomial == "Alectoris rufa")


mesocarnivores <- rbind(genetta, vulpes, meles, herpestes)



detections <- count(meso_data, vars = ind, wt_var = binomial)

# common genet
det_gen <- count(genetta, vars = ind, wt_var = binomial)
det_gen <- rename(det_gen, ind = vars, "Genetta genetta"= n)
det_gen <- det_gen[,c(1,3)]
spatial_data <- full_join(coor_data, det_gen , by = "ind")

#red fox
det_fox <- count(vulpes, vars = ind, wt_var = binomial)
det_fox<- rename(det_fox, ind = vars, "Vulpes vulpes"= n)
det_fox <- det_fox[,c(1,3)]

spatial_data <- full_join(spatial_data, det_fox , by = "ind")

# european badger
det_badger <- count(meles, vars = ind, wt_var = binomial)
det_badger<- rename(det_badger, ind = vars, "Meles meles"= n)
det_badger <- det_badger[,c(1,3)]

spatial_data <- full_join(spatial_data, det_badger , by = "ind")

# egyptian mongoose
det_mongoose <- count(herpestes, vars = ind, wt_var = binomial)
det_mongoose<- rename(det_mongoose, ind = vars, "Herpestes ichneumon"= n)
det_mongoose <- det_mongoose[,c(1,3)]

spatial_data <- full_join(spatial_data, det_mongoose , by = "ind")



#rabbit

det_rabbit<- count(rabbit, vars = ind, wt_var = binomial)
det_rabbit<- rename(det_rabbit, ind = vars, "Oryctolagus cunniculus"= n)
det_rabbit <- det_rabbit[,c(1,3)]
spatial_data <- full_join(spatial_data, det_rabbit , by = "ind")

#rodent

det_rodent<- count(rodent, vars = ind, wt_var = binomial)
det_rodent<- rename(det_rodent, ind = vars, "Small mammals"= n)
det_rodent <- det_rodent[,c(1,3)]
spatial_data <- full_join(spatial_data, det_rodent , by = "ind")

#partridge

det_partridge<- count(partridge, vars = ind, wt_var = binomial)
det_partridge<- rename(det_partridge, ind = vars, "Alectoris rufa"= n)
det_partridge <- det_partridge[,c(1,3)]
spatial_data <- full_join(spatial_data, det_partridge , by = "ind")

spatial_data <- spatial_data[1:105,]

#Replacing NA values
spatial_data$`Genetta genetta` <- ifelse(is.na(spatial_data$`Genetta genetta`), 0, spatial_data$`Genetta genetta`)
spatial_data$`Vulpes vulpes` <- ifelse(is.na(spatial_data$`Vulpes vulpes`), 0, spatial_data$`Vulpes vulpes`)
spatial_data$`Meles meles` <- ifelse(is.na(spatial_data$`Meles meles`), 0, spatial_data$`Meles meles`)
spatial_data$`Herpestes ichneumon` <- ifelse(is.na(spatial_data$`Herpestes ichneumon`), 0, spatial_data$`Herpestes ichneumon`)
spatial_data$`Oryctolagus cunniculus` <- ifelse(is.na(spatial_data$`Oryctolagus cunniculus`), 0, spatial_data$`Oryctolagus cunniculus`)
spatial_data$`Small mammals` <- ifelse(is.na(spatial_data$`Small mammals`), 0, spatial_data$`Small mammals`)
spatial_data$`Alectoris rufa` <- ifelse(is.na(spatial_data$`Alectoris rufa`), 0, spatial_data$`Alectoris rufa`)


write.csv(spatial_data,"Spatial_data.csv")

#import camera-days to spatial data.csv

effort_data <- read.csv("Total days camera.csv", header = T, sep=";")
spatial_data <- full_join(spatial_data, effort_data , by = "ind")
spatial_data <- spatial_data[1:105,]


write.csv(spatial_data,"Spatial_data.csv")

write_xlsx(spatial_data,"spatial_data.xlsx")


install.packages("writexl")
library("writexl")


# SUBSET PER POPULATION

SABCOL_det <- subset(spatial_data, area=="SABCOL")
SABOJI_det <- subset(spatial_data, area=="SABOJILL")
SABMAR_det <- subset(spatial_data, area =="SABMAR")

ggplot(SABCOL_det, aes(x= y , y=x)) + geom_point()
ggplot(SABOJI_det, aes(x= y , y=x)) + geom_point()
ggplot(SABMAR_det, aes(x= y , y=x)) + geom_point()


# plotting camera traps locations per population


?register_google

register_google(key = "AIzaSyBywAaRZNqOT-yzxi558JP_rDm47Tuiq-4") #API


sabcol_map <- get_map(location = c(lon = -6.50625, lat = 37.0004),zoom = 19, maptype = "satellite") 

ggmap(sabcol_map, base_layer = ggplot(SABCOL_det, aes(y , x))) + geom_point(size= 2.5, colour ="#FFB266") +
  xlab("Longitude") + ylab("Latitude") 

ggmap(sabcol_map, base_layer = ggplot(SABCOL_det, aes(y , x, colour = ind))) + geom_point(size= 2.5)



saboji_map <- get_map(location = c(lon = -6.51350, lat = 36.99550),zoom = 19, maptype = "satellite")
ggmap(saboji_map, base_layer = ggplot(SABOJI_det, aes(y=x , x=y))) + geom_point(size= 3, colour ="#CCFFE5") +
  xlab("Longitude") + ylab("Latitude")

sabmar_map <- get_map(location = c(lon = -6.5327, lat = 37.0122),zoom = 19, maptype = "satellite")
ggmap(sabmar_map, base_layer = ggplot(SABMAR_det, aes(x=y , y=x))) + geom_point(size= 3, colour ="#f2eb6b") +
  xlab("Longitude") + ylab("Latitude")


# LOCATIONS OF EACH AREA: SABCOL, SABMAR AND SABOJI IN DOÑANA NATIONAL PARK

doñana_map <- get_map(location = c(lon = -6.53, lat = 36.95),zoom = 12, maptype = "satellite") 
ggmap(doñana_map)

areas_map <- get_map(location = c(lon = -6.53, lat = 37),zoom = 14, maptype = "satellite") 


cord_areas<- read.csv("coord_areas.csv", header=T, sep= ";")

ggmap(areas_map, base_layer = ggplot(cord_areas, aes(x=lon , y=lat, colour=area))) + geom_point(size= 2.5) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Locations of each area")

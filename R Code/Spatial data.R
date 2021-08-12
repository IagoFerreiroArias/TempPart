# SCRIPT SPATIAL ANALYSIS
library(dplyr)
library(ggplot2)
library(tmap)
library(mapview)
library(sf)
library(ggmap)
library(ggsn)
library(viridis)
library(akima)
library(RColorBrewer)
library(maptools)
library(sp)
library(spaa)
library(bibtex)
write.bib(citation("spaa"))
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

?scalebar
?register_google

register_google(key = "AIzaSyBywAaRZNqOT-yzxi558JP_rDm47Tuiq-4") #API

Doñana <- get_map(location = c(lon = -6.4102800, lat = 36.940),zoom = 11, maptype = "satellite")
limits_doñana <- getKMLcoordinates("PN de Doñana.kml")
limits_doñana  <- lapply(1:length(limits_doñana), function(x) data.frame(limits_doñana[[x]], id = x))
limits_doñana  <- do.call(rbind, limits_doñana)
limits_doñana  <- rename(limits_doñana, lon = X1, lat = X2)


ggmap(Doñana, base_layer = ggplot(cord_areas, aes(lon , lat))) + geom_point(size= 2.5, colour ="#FFB266") +
  xlab("Longitude") + ylab("Latitude") + geom_polygon(data=limits_doñana, aes(x=lon, y=lat)) +
  ggsn::scalebar(x.min = -6.2, x.max = -6.6, dist_unit = "km", y.min = 36.8, y.max = 37.1, transform = T,
                                                      dist = 10, dd2km = TRUE, model = 'WGS84', location = "bottomleft")+ theme_classic()

sabcol_map <- get_map(location = c(lon = -6.50625, lat = 37.0004),zoom = 19, maptype = "satellite") 


SABCOL_det  <- rename(SABCOL_det, lon = y, lat = x)
ggmap(sabcol_map, base_layer = ggplot(SABCOL_det, aes(lon , lat))) + geom_point(size= 2.5, colour="FFEF65") +
  xlab("Longitude") + ylab("Latitude") + ggsn::scalebar(x.min = -6.506738, x.max = -6.50578, dist_unit = "m", y.min = 37, y.max = 37.0008, transform = T,
                                                        dist = 25, dd2km = TRUE, model = 'WGS84', location = "bottomleft") + theme_classic()

min(SABCOL_det$y)
ggmap(sabcol_map, base_layer = ggplot(SABCOL_det, aes(y , x, colour = "#FFFACD"))) + geom_point(size= 2.5) + 
  xlab("Longitude") + ylab("Latitude") + labs(title="Sabinar de Colonización (SABCOL)")
install.packages("htmltools")

saboji_map <- get_map(location = c(lon = -6.51350, lat = 36.99550),zoom = 19, maptype = "satellite")
ggmap(saboji_map, base_layer = ggplot(SABOJI_det, aes(y=x , x=y))) + geom_point(size= 3, colour ="#CCFFE5") +
  xlab("Longitude") + ylab("Latitude") +  ggsn::scalebar(x.min = -6.51395, x.max = -6.51294, dist_unit = "m", y.min = 36.995, y.max = 36.9959, transform = T,
                                                          dist = 25, dd2km = TRUE, model = 'WGS84', location = "bottomleft") + theme_classic()
max(SABOJI_det$x)
min(SABOJI_det$x)
max(SABOJI_det$y)
min(SABOJI_det$y)

sabmar_map <- get_map(location = c(lon = -6.5327, lat = 37.0122),zoom = 19, maptype = "satellite")
max(SABMAR_det$x)
min(SABMAR_det$x)
max(SABMAR_det$y)
min(SABMAR_det$y)
ggmap(sabmar_map, base_layer = ggplot(SABMAR_det, aes(x=y , y=x))) + geom_point(size= 3, colour ="orange1") +
  xlab("Longitude") + ylab("Latitude") + ggsn::scalebar(x.min = -6.5333, x.max = -6.53205, dist_unit = "m", y.min = 37.0117, y.max = 37.0127, transform = T,
                                                         dist = 25, dd2km = TRUE, model = 'WGS84', location = "bottomleft") + theme_classic()



# LOCATIONS OF EACH AREA: SABCOL, SABMAR AND SABOJI IN DONHANA NATIONAL PARK

donhana_map <- get_map(location = c(lon = -6.53, lat = 36.95),zoom = 12, maptype = "satellite") 
ggmap(donhana_map)

areas_map <- get_map(location = c(lon = -6.53, lat = 37),zoom = 14, maptype = "satellite") 


cord_areas<- read.csv("coord_areas.csv", header=T, sep= ";")

ggmap(areas_map, base_layer = ggplot(cord_areas, aes(x=lon , y=lat, colour=area))) + geom_point(size= 2.5) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Locations of each area")


# HEATMAP DETECTIONS USING SPATIAL DATA DATASET

# Estimating RAI for each species and for each camera trap station

# RAI = (detections/days)*100

#RAI for predators
spatial_data <- spatial_data %>% rename(Latitude = x, Longitude= y)
spatial_data$RAI_Genetta <- (spatial_data$`Genetta genetta`/spatial_data$totaldays) * 100
spatial_data$RAI_Vulpes <- (spatial_data$`Vulpes vulpes`/spatial_data$totaldays) * 100
spatial_data$RAI_Meles <- (spatial_data$`Meles meles`/spatial_data$totaldays) * 100
spatial_data$RAI_Herpestes <- (spatial_data$`Herpestes ichneumon`/spatial_data$totaldays) * 100

#RAI for preys

spatial_data$RAI_Rabbit <- (spatial_data$`Oryctolagus cunniculus`/spatial_data$totaldays) * 100
spatial_data$RAI_SMammals <- (spatial_data$`Small mammals`/spatial_data$totaldays) * 100
spatial_data$RAI_ARufa <- (spatial_data$`Alectoris rufa`/spatial_data$totaldays) * 100

Spatial_SABCOL <- subset(spatial_data, area=="SABCOL")
Spatial_SABOJI <- subset(spatial_data, area=="SABOJILL")
Spatial_SABMAR <- subset(spatial_data, area=="SABMAR")

library(ggplot2)



# scale_x_continuous(expand = c(0, 0)) +  scale_y_continuous(expand = c(0, 0))

class(Spatial_SABCOL)
str(Spatial_SABCOL)

ggmap(sabcol_map, base_layer = ggplot(Spatial_SABCOL, aes(x=Longitude , y=Latitude, z= RAI_Vulpes))) + xlab("Longitude") + ylab("Latitude") + labs(title="Sabinar de Colonización (SABCOL)") + 
    geom_contour_filled(data=da, min(Longitude), max(Longitude)) + scale_colour_viridis() + geom_point(size=2) 



ggplot(Spatial_SABCOL, aes(x=Longitude, y= Latitude)) + geom_point(size=4, alpha=0.9) + theme_classic()
ggplot(Spatial_SABOJI, aes(x=Longitude, y= Latitude)) + geom_point(size=4, alpha=0.9) + theme_classic()
ggplot(Spatial_SABMAR, aes(x=Longitude, y= Latitude)) + geom_point(size=4, alpha=0.9) + theme_classic()

# RAI MAPS: RED FOX 

# FOR SABCOL:
grid_fox_col <- interp(Spatial_SABCOL$Longitude, Spatial_SABCOL$Latitude, Spatial_SABCOL$RAI_Vulpes)
df_grid_fox_col <- data.frame(Longitude = rep(grid_fox_col$x, ncol(grid_fox_col$z)), 
                                        Latitude = rep(grid_fox_col$y, each = nrow(grid_fox_col$z)), 
                                        RAI_Vulpes = as.numeric(grid_fox_col$z))

df_grid_fox_col$RAI_Vulpes <- ifelse(is.na(df_grid_fox_col$RAI_Vulpes), 0, df_grid_fox_col$RAI_Vulpes)

max(df_grid_fox_col$RAI_Vulpes)
?scale_fill_viridis

ggplot(data = df_grid_fox_col, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Vulpes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 
  

ggplot(Spatial_SABCOL, aes(x=Longitude, y= Latitude)) + geom_point(size=4, alpha=0.9) + theme_classic()

ggplot(Spatial_SABCOL, aes(x=Longitude, y=Latitude, fill=RAI_Vulpes)) + geom_tile() + scale_fill_viridis(limits=c(0, 15))

# FOR SABOJI:
grid_fox_oji <- interp(Spatial_SABOJI$Longitude, Spatial_SABOJI$Latitude, Spatial_SABOJI$RAI_Vulpes)
df_grid_fox_oji <- data.frame(Longitude = rep(grid_fox_oji$x, ncol(grid_fox_oji$z)), 
                              Latitude = rep(grid_fox_oji$y, each = nrow(grid_fox_oji$z)), 
                              RAI_Vulpes = as.numeric(grid_fox_oji$z)) 

df_grid_fox_oji$RAI_Vulpes <- ifelse(is.na(df_grid_fox_oji$RAI_Vulpes), 0, df_grid_fox_oji$RAI_Vulpes)




ggplot(data = df_grid_fox_oji, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Vulpes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


max(df_grid_fox_oji$RAI_Vulpes)
ggplot(Spatial_SABOJI, aes(x=Longitude, y=Latitude, fill=RAI_Vulpes)) + geom_tile() + scale_fill_viridis(limits=c(0, 7))

# FOR SABMAR:
?interp
grid_fox_mar <- interp(Spatial_SABMAR$Longitude, Spatial_SABMAR$Latitude, Spatial_SABMAR$RAI_Vulpes, duplicate="strip")
df_grid_fox_mar <- data.frame(Longitude = rep(grid_fox_mar$x, ncol(grid_fox_mar$z)), 
                              Latitude = rep(grid_fox_mar$y, each = nrow(grid_fox_mar$z)), 
                              RAI_Vulpes = as.numeric(grid_fox_mar$z)) 

df_grid_fox_mar$RAI_Vulpes <- ifelse(is.na(df_grid_fox_mar$RAI_Vulpes), 0, df_grid_fox_mar$RAI_Vulpes)




ggplot(data = df_grid_fox_mar, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Vulpes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


max(df_grid_fox_mar$RAI_Vulpes)
ggplot(Spatial_SABMAR, aes(x=Longitude, y=Latitude, fill=RAI_Vulpes)) + geom_tile() + scale_fill_viridis(limits=c(0, 7))

# RAI MAPS: GENETTA GENETTA

# FOR SABCOL:
grid_gen_col <- interp(Spatial_SABCOL$Longitude, Spatial_SABCOL$Latitude, Spatial_SABCOL$RAI_Genetta)
df_grid_gen_col <- data.frame(Longitude = rep(grid_gen_col$x, ncol(grid_gen_col$z)), 
                              Latitude = rep(grid_gen_col$y, each = nrow(grid_gen_col$z)), 
                              RAI_Genetta = as.numeric(grid_gen_col$z))

df_grid_gen_col$RAI_Genetta <- ifelse(is.na(df_grid_gen_col$RAI_Genetta), 0, df_grid_gen_col$RAI_Genetta)

max(df_grid_gen_col$RAI_Genetta)


ggplot(data = df_grid_gen_col, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Genetta))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABCOL, aes(x=Longitude, y=Latitude, fill=RAI_Genetta)) + geom_tile() + scale_fill_viridis(limits=c(0, 4))

# FOR SABOJI:
grid_gen_oji <- interp(Spatial_SABOJI$Longitude, Spatial_SABOJI$Latitude, Spatial_SABOJI$RAI_Genetta)
df_grid_gen_oji <- data.frame(Longitude = rep(grid_gen_oji$x, ncol(grid_gen_oji$z)), 
                              Latitude = rep(grid_gen_oji$y, each = nrow(grid_gen_oji$z)), 
                              RAI_Genetta = as.numeric(grid_gen_oji$z))

df_grid_gen_oji$RAI_Genetta <- ifelse(is.na(df_grid_gen_oji$RAI_Genetta), 0, df_grid_gen_oji$RAI_Genetta)

max(df_grid_gen_oji$RAI_Genetta)


ggplot(data = df_grid_gen_oji, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Genetta))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABOJI, aes(x=Longitude, y=Latitude, fill=RAI_Genetta)) + geom_tile() + scale_fill_viridis(limits=c(0, 2))

# FOR SABMAR:
grid_gen_mar <- interp(Spatial_SABMAR$Longitude, Spatial_SABMAR$Latitude, Spatial_SABMAR$RAI_Genetta, duplicate="strip")
df_grid_gen_mar <- data.frame(Longitude = rep(grid_gen_mar$x, ncol(grid_gen_mar$z)), 
                              Latitude = rep(grid_gen_mar$y, each = nrow(grid_gen_mar$z)), 
                              RAI_Genetta = as.numeric(grid_gen_mar$z))

df_grid_gen_mar$RAI_Genetta <- ifelse(is.na(df_grid_gen_mar$RAI_Genetta), 0, df_grid_gen_mar$RAI_Genetta)

max(df_grid_gen_mar$RAI_Genetta)


ggplot(data = df_grid_gen_mar, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Genetta))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABMAR, aes(x=Longitude, y=Latitude, fill=RAI_Genetta)) + geom_tile() + scale_fill_viridis(limits=c(0, 1.5))

# RAI MAPS: MELES MELES

# FOR SABCOL:
grid_meles_col <- interp(Spatial_SABCOL$Longitude, Spatial_SABCOL$Latitude, Spatial_SABCOL$RAI_Meles)
df_grid_meles_col <- data.frame(Longitude = rep(grid_meles_col$x, ncol(grid_meles_col$z)), 
                              Latitude = rep(grid_meles_col$y, each = nrow(grid_meles_col$z)), 
                              RAI_Meles = as.numeric(grid_meles_col$z))

df_grid_meles_col$RAI_Meles <- ifelse(is.na(df_grid_meles_col$RAI_Meles), 0, df_grid_meles_col$RAI_Meles)

max(df_grid_meles_col$RAI_Meles)


ggplot(data = df_grid_meles_col, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Meles))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABCOL, aes(x=Longitude, y=Latitude, fill=RAI_Meles)) + geom_tile() + scale_fill_viridis(limits=c(0, 2))

# FOR SABOJI:
grid_meles_oji <- interp(Spatial_SABOJI$Longitude, Spatial_SABOJI$Latitude, Spatial_SABOJI$RAI_Meles)
df_grid_meles_oji <- data.frame(Longitude = rep(grid_meles_oji$x, ncol(grid_meles_oji$z)), 
                                Latitude = rep(grid_meles_oji$y, each = nrow(grid_meles_oji$z)), 
                                RAI_Meles = as.numeric(grid_meles_oji$z))

df_grid_meles_oji$RAI_Meles <- ifelse(is.na(df_grid_meles_oji$RAI_Meles), 0, df_grid_meles_oji$RAI_Meles)

max(df_grid_meles_oji$RAI_Meles)


ggplot(data = df_grid_meles_oji, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Meles))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABOJI, aes(x=Longitude, y=Latitude, fill=RAI_Meles)) + geom_tile() + scale_fill_viridis(limits=c(0, 2.5))

# FOR SABMAR:
grid_meles_mar <- interp(Spatial_SABMAR$Longitude, Spatial_SABMAR$Latitude, Spatial_SABMAR$RAI_Meles, duplicate="strip")
df_grid_meles_mar <- data.frame(Longitude = rep(grid_meles_mar$x, ncol(grid_meles_mar$z)), 
                                Latitude = rep(grid_meles_mar$y, each = nrow(grid_meles_mar$z)), 
                                RAI_Meles = as.numeric(grid_meles_mar$z))

df_grid_meles_mar$RAI_Meles <- ifelse(is.na(df_grid_meles_mar$RAI_Meles), 0, df_grid_meles_mar$RAI_Meles)

max(df_grid_meles_mar$RAI_Meles)


ggplot(data = df_grid_meles_mar, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Meles))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABMAR, aes(x=Longitude, y=Latitude, fill=RAI_Meles)) + geom_tile() + scale_fill_viridis(limits=c(0, 2.5))

# RAI MAPS: HERPESTES ICHNEUMON

# FOR SABCOL:
grid_herpestes_col <- interp(Spatial_SABCOL$Longitude, Spatial_SABCOL$Latitude, Spatial_SABCOL$RAI_Herpestes)
df_grid_herpestes_col <- data.frame(Longitude = rep(grid_herpestes_col$x, ncol(grid_herpestes_col$z)), 
                                Latitude = rep(grid_herpestes_col$y, each = nrow(grid_herpestes_col$z)), 
                                RAI_Herpestes = as.numeric(grid_herpestes_col$z))

df_grid_herpestes_col$RAI_Herpestes <- ifelse(is.na(df_grid_herpestes_col$RAI_Herpestes), 0, df_grid_herpestes_col$RAI_Herpestes)

max(df_grid_herpestes_col$RAI_Herpestes)


ggplot(data = df_grid_herpestes_col, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Herpestes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABCOL, aes(x=Longitude, y=Latitude, fill=RAI_Herpestes)) + geom_tile() + scale_fill_viridis(limits=c(0, 4))

# FOR SABOJI:
grid_herpestes_oji <- interp(Spatial_SABOJI$Longitude, Spatial_SABOJI$Latitude, Spatial_SABOJI$RAI_Herpestes)
df_grid_herpestes_oji <- data.frame(Longitude = rep(grid_herpestes_oji$x, ncol(grid_herpestes_oji$z)), 
                                    Latitude = rep(grid_herpestes_oji$y, each = nrow(grid_herpestes_oji$z)), 
                                    RAI_Herpestes = as.numeric(grid_herpestes_oji$z))

df_grid_herpestes_oji$RAI_Herpestes <- ifelse(is.na(df_grid_herpestes_oji$RAI_Herpestes), 0, df_grid_herpestes_oji$RAI_Herpestes)

max(df_grid_herpestes_oji$RAI_Herpestes)


ggplot(data = df_grid_herpestes_oji, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Herpestes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABOJI, aes(x=Longitude, y=Latitude, fill=RAI_Herpestes)) + geom_tile() + scale_fill_viridis(limits=c(0, 8.5))

# FOR SABMAR:
grid_herpestes_mar <- interp(Spatial_SABMAR$Longitude, Spatial_SABMAR$Latitude, Spatial_SABMAR$RAI_Herpestes, duplicate="strip")
df_grid_herpestes_mar <- data.frame(Longitude = rep(grid_herpestes_mar$x, ncol(grid_herpestes_mar$z)), 
                                    Latitude = rep(grid_herpestes_mar$y, each = nrow(grid_herpestes_mar$z)), 
                                    RAI_Herpestes = as.numeric(grid_herpestes_mar$z))

df_grid_herpestes_mar$RAI_Herpestes <- ifelse(is.na(df_grid_herpestes_mar$RAI_Herpestes), 0, df_grid_herpestes_mar$RAI_Herpestes)

max(df_grid_herpestes_mar$RAI_Herpestes)


ggplot(data = df_grid_herpestes_mar, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Herpestes))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABMAR, aes(x=Longitude, y=Latitude, fill=RAI_Herpestes)) + geom_tile() + scale_fill_viridis(limits=c(0, 4))

################################################################################################

# RAI MAPS OF PREY ITEMS

#################################################################################################


# RAI MAPS: ORYCTOLAGUS CUNNICULUS

# FOR SABCOL:
grid_rabbit_col <- interp(Spatial_SABCOL$Longitude, Spatial_SABCOL$Latitude, Spatial_SABCOL$RAI_Rabbit)
df_grid_rabbit_col <- data.frame(Longitude = rep(grid_rabbit_col$x, ncol(grid_rabbit_col$z)), 
                                    Latitude = rep(grid_rabbit_col$y, each = nrow(grid_rabbit_col$z)), 
                                    RAI_Rabbit = as.numeric(grid_rabbit_col$z))

df_grid_rabbit_col$RAI_Rabbit <- ifelse(is.na(df_grid_rabbit_col$RAI_Rabbit), 0, df_grid_rabbit_col$RAI_Rabbit)

max(df_grid_rabbit_col$RAI_Rabbit)


ggplot(data = df_grid_rabbit_col, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Rabbit))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABCOL, aes(x=Longitude, y=Latitude, fill=RAI_Rabbit)) + geom_tile() + scale_fill_viridis(limits=c(0, 3))

# FOR SABOJI:
grid_rabbit_oji <- interp(Spatial_SABOJI$Longitude, Spatial_SABOJI$Latitude, Spatial_SABOJI$RAI_Rabbit)
df_grid_rabbit_oji <- data.frame(Longitude = rep(grid_rabbit_oji$x, ncol(grid_rabbit_oji$z)), 
                                 Latitude = rep(grid_rabbit_oji$y, each = nrow(grid_rabbit_oji$z)), 
                                 RAI_Rabbit = as.numeric(grid_rabbit_oji$z))

df_grid_rabbit_oji$RAI_Rabbit <- ifelse(is.na(df_grid_rabbit_oji$RAI_Rabbit), 0, df_grid_rabbit_oji$RAI_Rabbit)

max(df_grid_rabbit_oji$RAI_Rabbit)


ggplot(data = df_grid_rabbit_oji, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Rabbit))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABOJI, aes(x=Longitude, y=Latitude, fill=RAI_Rabbit)) + geom_tile() + scale_fill_viridis(limits=c(0, 45))

# FOR SABMAR:
grid_rabbit_mar <- interp(Spatial_SABMAR$Longitude, Spatial_SABMAR$Latitude, Spatial_SABMAR$RAI_Rabbit, duplicate="strip")
df_grid_rabbit_mar <- data.frame(Longitude = rep(grid_rabbit_mar$x, ncol(grid_rabbit_mar$z)), 
                                 Latitude = rep(grid_rabbit_mar$y, each = nrow(grid_rabbit_mar$z)), 
                                 RAI_Rabbit = as.numeric(grid_rabbit_mar$z))

df_grid_rabbit_mar$RAI_Rabbit <- ifelse(is.na(df_grid_rabbit_mar$RAI_Rabbit), 0, df_grid_rabbit_mar$RAI_Rabbit)

max(df_grid_rabbit_mar$RAI_Rabbit)


ggplot(data = df_grid_rabbit_mar, aes(x = Longitude,y = Latitude,z =as.numeric(RAI_Rabbit))) +
  geom_contour_filled(aes(fill=..level..)) + 
  scale_fill_viridis_d(drop = FALSE, alpha=0.8) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(axis.text.y=element_text(angle=90)) 


ggplot(Spatial_SABMAR, aes(x=Longitude, y=Latitude, fill=RAI_Rabbit)) + geom_tile() + scale_fill_viridis(limits=c(0, 90))



# Spatial overlap: Pianka index

S_Overlap_data <- rbind(Spatial_SABCOL, Spatial_SABOJI, Spatial_SABMAR)

# SITES GROUPED:

SO_GV <- niche.overlap.boot.pair(S_Overlap_data$RAI_Genetta, S_Overlap_data$RAI_Vulpes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_GM <- niche.overlap.boot.pair(S_Overlap_data$RAI_Genetta, S_Overlap_data$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_GH <-niche.overlap.boot.pair(S_Overlap_data$RAI_Genetta, S_Overlap_data$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_VM <-niche.overlap.boot.pair(S_Overlap_data$RAI_Vulpes, S_Overlap_data$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_VH <-niche.overlap.boot.pair(S_Overlap_data$RAI_Vulpes, S_Overlap_data$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_MH <- niche.overlap.boot.pair(S_Overlap_data$RAI_Meles, S_Overlap_data$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_df <- data.frame( SO_GV, SO_GM, SO_GH, SO_VM, SO_VH, SO_MH)
SO_df<- t(SO_df)
SO_df<- cbind(SO_df, Sp_pairs = c("GV", "GM", "GH", "VM", "VH", "MH"))
SO_df <- as.data.frame(SO_df)
str(SO_df)
SO_df$Observed <- as.numeric(SO_df$Observed)
SO_df$`Boot CI1` <- as.numeric(SO_df$`Boot CI1`)
SO_df$`Boot CI2` <- as.numeric(SO_df$`Boot CI2`)

ggplot(SO_df, aes(Sp_pairs, Observed, ymin = `Boot CI1` , ymax = `Boot CI2`)) + 
  geom_pointrange() + theme_classic() + labs(x="Species pairs", y="Pianka's index") + scale_y_continuous(limits = c(0,1))


#SPATIAL OVERLAP IN SABCOL:

SO_GV_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Genetta, Spatial_SABCOL$RAI_Vulpes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_GM_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Genetta, Spatial_SABCOL$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_GH_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Genetta, Spatial_SABCOL$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_VM_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Vulpes, Spatial_SABCOL$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_VH_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Vulpes, Spatial_SABCOL$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_MH_SABCOL <- niche.overlap.boot.pair(Spatial_SABCOL$RAI_Meles, Spatial_SABCOL$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))
 
SO_SABCOL_df <- data.frame( SO_GV_SABCOL, SO_GM_SABCOL, SO_GH_SABCOL, SO_VM_SABCOL, SO_VH_SABCOL, SO_MH_SABCOL)
SO_SABCOL_df<- t(SO_SABCOL_df)
SO_SABCOL_df<- cbind(SO_SABCOL_df, Sp_pairs = c("GV", "GM", "GH", "VM", "VH", "MH"))
SO_SABCOL_df <- as.data.frame(SO_SABCOL_df)
str(SO_SABCOL_df)
SO_SABCOL_df$Observed <- as.numeric(SO_SABCOL_df$Observed)
SO_SABCOL_df$`Boot CI1` <- as.numeric(SO_SABCOL_df$`Boot CI1`)
SO_SABCOL_df$`Boot CI2` <- as.numeric(SO_SABCOL_df$`Boot CI2`)

ggplot(SO_SABCOL_df, aes(Sp_pairs, Observed, ymin = `Boot CI1` , ymax = `Boot CI2`)) + 
  geom_pointrange() + theme_classic() + labs(x="Species pairs", y="Pianka's index") + scale_y_continuous(limits = c(0,1))


#SPATIAL OVERLAP IN SABOJI:
SO_GV_SABOJI <- niche.overlap.boot.pair(Spatial_SABOJI$RAI_Genetta, Spatial_SABOJI$RAI_Vulpes, method = c("pianka"),
                                        times = 1000, quant = c(0.025, 0.975))
  
SO_GM_SABOJI <- niche.overlap.boot.pair(Spatial_SABOJI$RAI_Genetta, Spatial_SABOJI$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_GH_SABOJI <- niche.overlap.boot.pair(Spatial_SABOJI$RAI_Genetta, Spatial_SABOJI$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_VM_SABOJI <- niche.overlap.boot.pair(Spatial_SABOJI$RAI_Vulpes, Spatial_SABOJI$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))



SO_VH_SABOJI <- niche.overlap.boot.pair(Spatial_SABOJI$RAI_Vulpes, Spatial_SABOJI$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_MH_SABOJI <-niche.overlap.boot.pair(Spatial_SABOJI$RAI_Meles, Spatial_SABOJI$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_SABOJI_df <- data.frame(SO_GV_SABOJI, SO_GM_SABOJI, SO_GH_SABOJI, SO_VM_SABOJI, SO_VH_SABOJI, SO_MH_SABOJI)
SO_SABOJI_df<- t(SO_SABOJI_df)
SO_SABOJI_df<- cbind(SO_SABOJI_df, Sp_pairs = c("GV", "GM", "GH", "VM", "VH", "MH"))
SO_SABOJI_df <- as.data.frame(SO_SABOJI_df)
str(SO_SABOJI_df)
SO_SABOJI_df$Observed <- as.numeric(SO_SABOJI_df$Observed)
SO_SABOJI_df$`Boot CI1` <- as.numeric(SO_SABOJI_df$`Boot CI1`)
SO_SABOJI_df$`Boot CI2` <- as.numeric(SO_SABOJI_df$`Boot CI2`)

ggplot(SO_SABOJI_df, aes(Sp_pairs, Observed, ymin = `Boot CI1` , ymax = `Boot CI2`)) + 
  geom_pointrange() + theme_classic() + labs(x="Species pairs", y="Pianka's index") + scale_y_continuous(limits = c(0,1))

#SPATIAL OVERLAP IN SABMAR:

SO_GV_SABMAR <- niche.overlap.boot.pair(Spatial_SABMAR$RAI_Genetta, Spatial_SABMAR$RAI_Vulpes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_GM_SABMAR <- niche.overlap.boot.pair(Spatial_SABMAR$RAI_Genetta, Spatial_SABMAR$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_GH_SABMAR <- niche.overlap.boot.pair(Spatial_SABMAR$RAI_Genetta, Spatial_SABMAR$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_VM_SABMAR <- niche.overlap.boot.pair(Spatial_SABMAR$RAI_Vulpes, Spatial_SABMAR$RAI_Meles, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))


SO_VH_SABMAR <-niche.overlap.boot.pair(Spatial_SABMAR$RAI_Vulpes, Spatial_SABMAR$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_MH_SABMAR <- niche.overlap.boot.pair(Spatial_SABMAR$RAI_Meles, Spatial_SABMAR$RAI_Herpestes, method = c("pianka"),
                        times = 1000, quant = c(0.025, 0.975))

SO_SABMAR_df <- data.frame(SO_GV_SABMAR, SO_GM_SABMAR, SO_GH_SABMAR, SO_VM_SABMAR, SO_VH_SABMAR, SO_MH_SABMAR)
SO_SABMAR_df<- t(SO_SABMAR_df)
SO_SABMAR_df<- cbind(SO_SABMAR_df, Sp_pairs = c("GV", "GM", "GH", "VM", "VH", "MH"))
SO_SABMAR_df <- as.data.frame(SO_SABMAR_df)
str(SO_SABMAR_df)
SO_SABMAR_df$Observed <- as.numeric(SO_SABMAR_df$Observed)
SO_SABMAR_df$`Boot CI1` <- as.numeric(SO_SABMAR_df$`Boot CI1`)
SO_SABMAR_df$`Boot CI2` <- as.numeric(SO_SABMAR_df$`Boot CI2`)

ggplot(SO_SABMAR_df, aes(Sp_pairs, Observed, ymin = `Boot CI1` , ymax = `Boot CI2`)) + 
  geom_pointrange() + theme_classic() + labs(x="Species pairs", y="Pianka's index") + scale_y_continuous(limits = c(0,1))


#vegetation transects

veg_data <- read.csv("transects.csv", skip=12 ,header=T, sep= ";")
str(veg_data)
veg_data$site <- as.factor(veg_data$site)
veg_data$transect <- as.factor(veg_data$transect)
veg_data$species <- as.factor(veg_data$species)
veg_data$cover <- as.numeric(veg_data$cover)
veg_data$cover_m <- veg_data$cover/100

open_sabcol <-veg_data %>% filter(site=="SABCOL", species=="Open") %>% group_by(transect) %>% summarise(mean = mean(cover_m,na.rm=T ))
open_sabcol$perct <-(open_sabcol$mean/50)*100
100-mean(open_sabcol$perct)
100-sd(open_sabcol$perct)

open_saboji <-veg_data %>% filter(site=="SABOJI", species=="Open") %>% group_by(transect) %>% summarise(mean = mean(cover_m,na.rm=T ))
open_saboji$perct <-(open_saboji$mean/50)*100
100-mean(open_saboji$perct)
100-sd(open_saboji$perct)

open_sabmar <-veg_data %>% filter(site=="SABMAR", species=="Open") %>% group_by(transect) %>% summarise(mean = mean(cover_m,na.rm=T ))
open_sabmar$perct <-(open_sabmar$mean/50)*100
100-mean(open_sabmar$perct)
100-sd(open_sabmar$perct)



veg_data %>% filter(site=="SABCOL", species=="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))
veg_data %>% filter(site=="SABOJI", species=="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))
veg_data %>% filter(site=="SABMAR", species=="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))

veg_data %>% filter(site=="SABCOL", species!="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))
veg_data %>% filter(site=="SABOJI", species!="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))
veg_data %>% filter(site=="SABMAR", species!="Open") %>% summarise(mean = mean(cover_m,na.rm=T ), sd=sd(cover_m,na.rm=T))
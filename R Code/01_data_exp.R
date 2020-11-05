# data are circular, we need specific packages
#analyze data
#install.packages("activity", "circular", "camtrapR")
library(overlap)
library(activity)
library(circular)
library(camtrapR)
library(readr)
library(ggplot2)
library(dplyr)

#data camtraps
data<-readr::read_csv(file = "JPHO_phototraping.csv", skip=18)

# convert dates and times to POXIT format
data$date <- as.Date(data$date , "%d/%m/%Y") # convert to date
data$DateTimeOriginal <- paste0(data$date," ", data$time)
data$DateTimeOriginal <- as.POSIXct(data$DateTimeOriginal, tmz= "", format = "%Y-%m-%d %H:%M:%OS")

#fix some typos
data[data$species == "scorfa","species"]<-"scrofa"
data[data$species == "uknown","species"]<-"unknown"

data$binomial<-paste0(data$genus," ", data$species)
unique(data$binomial)
table(data$binomial, data$sp)

data<-data[data$binomial != "Unknown unknown",]

sp_abund_area<-meso_data %>% 
  group_by(area, sp)%>%
  summarise(count =n())
  
sp_area<-data %>% 
  group_by(area)%>%
  distinct(sp) %>%
  summarise(nsp =n())

#plot number species per area
ggplot(sp_area) + geom_point(aes(area,nsp), stat = "identity")

#load traits
mtraits<-readr::read_csv(file ="~/DATASETS/EltonTraits/M_Traits_guild.csv")
btraits<-readr::read_csv(file ="~/DATASETS/EltonTraits/B_traits2019.csv")

mdata<-inner_join(data,mtraits[,c("Species", "BM", "guild")], by = c("binomial" ="Species"))
bdata<-inner_join(data,btraits[,c("Species", "BM", "Diet_cat")], by = c("binomial" ="Species"))

colnames(mdata)[18]<-"Diet_cat"

data<-rbind(mdata,bdata)

#allmammals
ggplot(mdata) + geom_density(aes(time, fill = sp, colour=sp, alpha =I(.5))) +facet_grid(cols = vars(Diet_cat))

#only carnivores
carndata<-mdata[mdata$Diet_cat != "Herb",]
herbdata<-mdata[mdata$Diet_cat == "Herb",]

# plot
ggplot(carndata) + geom_density(aes(time, fill = sp, colour=sp, alpha =I(.5))) +
                  facet_grid(cols = vars(area))

ggplot(herbdata) + geom_density(aes(time, fill = sp, colour=sp, alpha =I(.5)))# +
  #facet_grid(cols = vars(area))

# explore number or records per species and area
table(carndata$area, carndata$binomial)
table(herbdata$area, herbdata$binomial)

# bird data (prey)
table(bdata$area, bdata$binomial)

ggplot(bdata) + geom_density(aes(time, fill = sp, colour=sp, alpha =I(.5)))# +
#facet_grid(cols = vars(area))

ale_ruf<-bdata[bdata$binomial == "Alectoris rufa",]

ggplot(ale_ruf) + geom_density(aes(time, fill = "red", colour="red", alpha =I(.5))) +
  facet_grid(cols = vars(area)) +theme(legend.position = "none")


# analyses with specific packages to treat circular stats
# density plot
#does not work
activityDensity(recordTable = carndata,
                species = "Meles meles",
                speciesCol = "binomial",
                recordDateTimeCol = "DateTimeOriginal",
                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                allSpecies = FALSE)

#circular plot, not quite yet
vul_vul<-data[data$binomial == "Vulpes vulpes",]
ggplot(vul_vul, aes(x = time)) + geom_histogram(breaks = seq(0,24), colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Events by Time of day")# + 
  #scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))

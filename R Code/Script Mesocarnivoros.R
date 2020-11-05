# We need specific packages: analyze data
#install.packages("activity", "circular", "camtrapR"...)
library(overlap)
library(activity)
library(circular)
library(camtrapR)
library(readr)
library(ggplot2)
library(dplyr)
library(hms) #time variabes
library(wesanderson)
library(astroFns) # time to radians
library(bibtex)

# Citations for manuscript
write.bib(citation("overlap"))
write.bib(citation("camtrapR"))
write.bib(citation("activity"))
write.bib(citation("circular"))


# Import dataset
raw_data <- read.csv("JPHO_phototraping_definitive.csv", skip = 18, header = T, sep = ";", as.is = T )

# New variable with species binomial names: genus + species

raw_data$binomial<-paste0(raw_data$genus," ", raw_data$species)
raw_data$binomial<-as.factor(raw_data$binomial)
levels(raw_data$binomial)

# New variable with hms
raw_data$hms <- as_hms(raw_data$time)

# Package overlap works entirely in radians.
# New variable: time in radians -> TimeRad

raw_data$TimeRad <- hms2rad(raw_data$hms)

# Format change variables: as.factor and as.date

raw_data$area <- as.factor(raw_data$area)
levels(raw_data$area)
raw_data$ind <- as.factor(raw_data$ind)
raw_data$sp <- as.factor(raw_data$sp)
raw_data$behavior <- as.factor(raw_data$behavior)
raw_data$date <- as.Date(raw_data$date,  "%d/%m/%Y")

# Creation of a new variable and POSIXct format
?as.POSIXt
raw_data$Date_Time <- paste0(raw_data$date," ", raw_data$time)
raw_data$Date_Time<- as.POSIXct(raw_data$Date_Time, format = "%Y-%m-%d %H:%M:%S")
class(raw_data$Date_Time)
summary(raw_data$Date_Time) #check if dates are comprised between 2018 and 2020

# Filter NA values in date and time variables. (remove?)
# There is setup error in 4 dates: SABOJI O334 cam07 test 4 (delete?)

# now that data are cleaned, we call it as meso_data

meso_data <- subset(raw_data, is.na(raw_data$Date_Time)==FALSE)


# As our study is carried out over 2 years, daylight vary according to the seasons 
# This fact could introduce biases. In order to avoid it we perform average anchoring

# solartime() transforms clock time to solar time anchored to sun rise and sunset times for a given location.




?solartime
solartime <- solartime(meso_data$Date_Time, lat= 37.0000000 , long= -6.3333300,tz= 1  ,"%Y-%m-%d %H:%M:%S")
solartime # $solar: radian solar time data anchored to average sun rise and sun set times.

meso_data$solar <- solartime$solar
meso_data$solar_input <- solartime$input

# class: circular -> necessary for MWW test
meso_data$solar_circ <- circular(meso_data$solar, units = "radians")
class(meso_data$solar_circ)


write.csv(meso_data, "meso_data.csv")
write.table(meso_data, "meso_data.xlsx")

#######################################################################################################################
#                                                                                                                     #
#                                    DETECTIONS OF EACH SPECIES PER POPULATION                                        #
#                                                                                                                     #
#######################################################################################################################

#MESOCARNIVORES (SUBSET PER SPECIES):
genetta <- subset(meso_data, binomial == "Genetta genetta")
vulpes <- subset(meso_data, binomial =="Vulpes vulpes")
meles <- subset(meso_data, binomial =="Meles meles")
herpestes <- subset(meso_data, binomial == "Herpestes ichneumon")
mesocarnivores <- rbind(genetta, vulpes, meles, herpestes)

# MESOCARNIVORE DETECTIONS PER POPULATION
summary(genetta$area)
summary(herpestes$area)
summary(vulpes$area)
summary(meles$area)


#PREYS (SUBSET PER SPECIES):
rabbit <- subset(meso_data, binomial == "Oryctolagus cuniculus")
rodent<- subset(meso_data, sp =="rodent")
zorzal <- subset(meso_data, binomial=="Turdus philomelos")
mirlo <- subset(meso_data, binomial =="Turdus merula")
perdiz <- subset(meso_data, binomial =="Alectoris rufa")


# PREY DETECTIONS PER POPULATION
summary(rabitt$area)
summary(rodent$area)
summary(zorzal$area)
summary(mirlo$area)
summary(perdiz$area)


########################################################################################################################
#                                                                                                                      #
#                                              PLOTS : GLOBAL MESOCARNIVORE OVERLAPPING                                #
#                                                                                                                      #
########################################################################################################################

# Activity Density plots for all species in dataset. WARNING: There is species in the dataset which number of detections are lower than 10
# so their respective kernel density plot would not be well estimated.

levels(meso_data$binomial)

activityDensity(recordTable = meso_data, allSpecies = TRUE, speciesCol = "binomial", recordDateTimeCol = "Date_Time",
                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", plotR = TRUE)


# In order to estimate kernel density plots and get overlapping plots for each pair of sp we use activityOverlap from camtrapR package
# Each specie has its colour:
# Herpestes ichneumon: #F0E68C (yellow)
# Vulpes vulpes: #4682B4 (blue)
# Genetta genetta: #66CDAA (green)
# Meles meles: #9370DB (purple)

?activityOverlap

# Activity overlaping for mesocarnivores: herpestes ichneumon vs vulpes vulpes. Double anchoring transformated values:

overlapPlot(herpestes$solar, vulpes$solar, rug =T,linecol = c("#F0E68C", "#4682B4"), ylim= c(0.00, 0.13),
            linewidth = c(1, 1), linetype = c(1,1), olapcol = "#FFFFE0", main="Egyptian mongoose vs Red fox")
abline(v=c(8, 20), lty=3) #EXAMPLE: plot with dotted lines for sunrise (8am) and sunset (20pm) # CHANGE THEM!

# Activity overlaping for mesocarnivores: herpestes ichneumon vs genetta genetta
overlapPlot(herpestes$solar, genetta$solar, rug =T,linetype = c(1, 1), linecol = c("#F0E68C", "#66CDAA"), ylim= c(0.00, 0.13),
            olapcol = "#F5FFFA", main="Egyptian mongoose vs Common genet")

# Activity overlaping for mesocarnivores: herpestes ichneumon vs meles meles
overlapPlot(herpestes$solar, meles$solar, rug =T,linecol = c("#F0E68C", "#9370DB"), linetype = c(1, 1),ylim= c(0.00, 0.13),
            olapcol = "#FFFFE0", linewidth = c(1, 1), main="Egyptian mongoose vs European badger")


# Activity overlaping for mesocarnivores: genetta genetta vs meles meles
overlapPlot(genetta$solar, meles$solar, rug =T,linetype = c(1,1),  linecol = c("#66CDAA", "#9370DB"), ylim= c(0.00, 0.13),
            olapcol = "#F5FFFA", linewidth = c(1, 1), main="Common genet vs European badger")

# Activity overlaping for mesocarnivores: genetta genetta vs vulpes vulpes
overlapPlot(genetta$solar, vulpes$solar, rug =T,linetype = c(1,1),linecol = c("#66CDAA", "#4682B4"), ylim= c(0.00, 0.13),
            olapcol = "#F5FFFA" , main="Common genet vs Red fox")

# Activity overlaping for mesocarnivores: vulpes vulpes vs meles meles
overlapPlot(vulpes$solar, meles$solar, rug =T,linetype = c(1,1), linecol = c("#4682B4", "#9370DB"),ylim= c(0.00, 0.13),
            olapcol = "#F0F8FF", main="Red fox vs European badger")


#######################################################################################################################
#                                                                                                                     #
#                                                    OVERLAPPING: PREDATOR - PREY                                     #
#                                                                                                                     #
#######################################################################################################################

levels(meso_data$binomial)

# OVERLAPPING BETWEEN RED FOX AND PREYS:

# Activity overlaping predator-prey: oryctolagus cuniculus


overlapPlot(rabbit$solar, vulpes$solar, rug =T,linecol = c("#4682B4", "#F5DEB3"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#B0C4DE", main="Rabbit vs Red fox")
min(length(rabbit$solar), length(vulpes$solar))
overlapEst(rabbit$solar, vulpes$solar)



overlapPlot(rabbit$solar, genetta$solar, rug =T,linecol = c("#4682B4", "#66CDAA"),  ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#B0C4DE", main="Rabbit vs Common genet")

overlapPlot(rabbit$solar, meles$solar, rug =T,linecol = c("#4682B4", "#9370DB"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#B0C4DE", main="Rabbit vs European badger")


overlapPlot(rabbit$solar, herpestes$solar, rug =T,linecol = c("#4682B4", "#F0E68C"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#B0C4DE", main="Rabbit vs Egyptian mongoose")


# Activity overlaping predator-prey: small mammals

overlapPlot(rodent$solar, meles$solar, rug =T,linecol = c("#008080", "#9370DB"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#F5FFFA", main="Small mammal vs European badger")

overlapPlot(rodent$solar, herpestes$solar, rug =T,linecol = c("#008080", "#F0E68C"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#F5FFFA", main="Small mammal vs Egyptian mongoose")

overlapPlot(rodent$solar, vulpes$solar, rug =T,linecol = c("#008080", "#4682B4"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#F5FFFA", main="Small mammal vs Red fox")

overlapPlot(rodent$solar, genetta$solar, rug =T,linecol = c("#008080", "#66CDAA"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#F5FFFA", main="Small mammal vs Common genet")


# Activity overlaping predator-prey: red-legged partridge




overlapPlot(perdiz$solar, vulpes$solar, rug =T,linecol = c("#DAA520", "#4682B4"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#FFFACD", main="Red-legged partridge vs Red fox")

overlapPlot(perdiz$solar, herpestes$solar, rug =T,linecol = c("#DAA520", "#F0E68C"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#FFFACD", main="Red-legged partridge vs Egyptian mongoose")

overlapPlot(perdiz$solar, genetta$solar, rug =T,linecol = c("#DAA520", "#66CDAA"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#FFFACD", main="Red-legged partridge vs Common genet")

overlapPlot(perdiz$solar, meles$solar, rug =T,linecol = c("#DAA520", "#9370DB"), ylim= c(0.00, 0.15),
            linewidth = c(1, 1), linetype = c(2,1), olapcol = "#FFFACD", main="Red-legged partridge vs European bager")





#########################################################################################################################################
#                                                                                                                                       #
#                                             OVERLAPPING COEFFICIENTS, CIs AND SIGNIFICANCE                                            #
#                                                                                                                                       #
#########################################################################################################################################

# Subsets for each sp were extracted previously (see DETECTIONS OF EACH SPECIES PER POPULATION)

# For each sp, we have different number of detections. Each overlap coefficient is related to a determined sample size so Dhat1 will be
# use when sample size < 50 and Dhat4 when sample size > 50. Thus, we need to check the number of detections for each sp and choose 
# the appropiate coefficient related to the lower number of records from both species.

# ¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡  CAUTION !!!!!!!!!!!!!!!!!!!!

# Function overlapEst from overlap package. Each vector has a code for each sp (Overlap_GV). Thus: Genneta = G, Vulpes = V, Meles = M, Herpestes = H
# Calculating the overlap with the three estimators: Dthat 1 , 4 and 5 (choose one checking the sample size first)
# To get the size of the smaller of the two samples, type:


# OVERLAP | Common genet vs Red fox:

min(length(genetta$TimeRad), length(vulpes$TimeRad)) # Sample size < 50 so we should use Dthat 1 (check because new detections could be added to the dataset)
overlap_GV <- overlapEst(genetta$TimeRad, vulpes$TimeRad)
overlap_GV
overlapEst(vulpes$solar, genetta$solar)
# OVERLAP | Common genet vs Egyptian moongose:
min(length(genetta$TimeRad), length(herpestes$TimeRad))
overlap_GH <- overlapEst(genetta$TimeRad, herpestes$TimeRad)
overlap_GH
overlapEst(genetta$solar,herpestes$solar)
# OVERLAP | Common genet vs European badger:
min(length(genetta$TimeRad), length(meles$TimeRad))
overlap_GM <- overlapEst(genetta$TimeRad, meles$TimeRad)
overlap_GM
overlapEst(genetta$solar, meles$solar)

# OVERLAP | Red fox vs Egyptian moongse:
min(length(vulpes$TimeRad), length(herpestes$TimeRad))
overlap_VH <- overlapEst(vulpes$TimeRad, herpestes$TimeRad)
overlap_VH
overlapEst(vulpes$solar, herpestes$solar)

# OVERLAP | Red fox vs European badger:
min(length(vulpes$TimeRad), length(meles$TimeRad))
overlap_VM <- overlapEst(vulpes$TimeRad, meles$TimeRad)
overlap_VM
overlapEst(vulpes$solar, meles$solar)

# OVERLAP | European badger vs Egyptian moongse:
min(length(meles$TimeRad), length(meles$TimeRad))
overlap_MH <- overlapEst(meles$TimeRad, herpestes$TimeRad)
overlap_MH
overlapEst(meles$solar, herpestes$solar)


# In order to estimate confidence interval of the previous coefficients of overlapping, a bootstrap analysis will be performed.
# Smoothed bootstrap will be used because usual bootsrap method assumes that the sample is fully representative of the populations.
# Thus, randomly resampling may not estimate well activity patterns. For example, if we take our original records from european badger,
# which is noctural, strict bootstrapp method will never yield and observation outside that range. 

# OVERLAP PACKAGE:  bootstraps are generated with resample and a smoothing argument can be specified: 
# if smooth = TRUE (the default), smoothed bootstraps are generated.

# IMPORTANT: 10,000 SMOOTHED BOTTSRAPS WILL BE GENERATED

foxboot <- resample(vulpes$solar, 10000) # This produces matrices with a column for each bootstrap sample. 
genetboot <- resample(genetta$solar, 10000)
badgerboot <- resample(meles$solar, 10000)
mongooseboot<- resample(herpestes$solar, 10000)

dim(foxboot) #The bootstrap sample size is the same as the original sample size.
dim(genetboot)
dim(badgerboot)
dim(mongooseboot)


rabbitboot <- resample(rabbit$solar, 10000) # This produces matrices with a column for each bootstrap sample. 
rodentboot <- resample(rodent$solar, 10000)
perdizboot <- resample(perdiz$solar, 10000)




# To generate estimates of the overlap from each pair of samples, these two matrices are passed to the function bootEst().
# Since the size of the smaller of the two samples is lower than 75 only Dhat1 should be considered; consequently, the
# estimation of the others can be suppressed by setting adjust = c(NA, 1, NA), which considerably reduces the computation time.

# CHECK FIRST THE NUMBER OF THE DETECTIONS

# BOOTSTRAP | Common genet vs Red fox:
boot_GV<-bootEst(genetboot, foxboot, adjust=c(1,NA,NA))
boot_GV
# BOOTSTRAP |  Common genet vs Egyptian moongose:
boot_GH<-bootEst(genetboot, mongooseboot, adjust=c(1,NA,NA))
boot_GH
# BOOTSTRAP |  Common genet vs European badger:
boot_GM<-bootEst(genetboot, badgerboot, adjust=c(1,NA,NA))
boot_GM
# BOOTSTRAP |  Red fox vs Egyptian moongse:
boot_VH<-bootEst(foxboot, mongooseboot, adjust=c(1,NA,NA))
boot_VH
# BOOTSTRAP |  Red fox vs European badger:
boot_VM<-bootEst(foxboot, badgerboot, adjust=c(1,NA,NA))
boot_VM
# BOOTSTRAP |  European badger vs Egyptian moongse:
boot_MH<-bootEst(badgerboot, mongooseboot, adjust=c(1,NA,NA))
boot_MH

# PREY - PREDATOR DELTAS, BOOTS AND CIs: R = rabbit S = small mammal P = partridge

# RABBIT vs FOX
overlap_RV <- overlapEst(rabbit$solar, vulpes$solar)
overlap_RV
boot_RV<-bootEst(rabbitboot, foxboot, adjust=c(NA,1,NA))
boot_RV
CI_RV <- bootCI(overlap_RV[2], boot_RV[,2], conf = 0.99)
CI_RV

# RABBIT VS BADGER
overlap_RM <- overlapEst(rabbit$solar, meles$solar)
overlap_RM
boot_RM<-bootEst(rabbitboot, badgerboot, adjust=c(1,NA,NA))
boot_RM
CI_RM <- bootCI(overlap_RM[1], boot_RM[,1], conf = 0.99)
CI_RM

# RABBIT VS GENET

overlap_RG <- overlapEst(rabbit$solar, genetta$solar)
overlap_RG
boot_RG<-bootEst(rabbitboot, genetboot, adjust=c(1,NA,NA))
boot_RG
CI_RG <- bootCI(overlap_RG[1], boot_RG[,1], conf = 0.99)
CI_RG

# RABBIT VS MONGOOSE

overlap_RH <- overlapEst(rabbit$solar, herpestes$solar)
overlap_RH

boot_RH<-bootEst(rabbitboot, mongooseboot, adjust=c(1,NA,NA))
boot_RH

CI_RH <- bootCI(overlap_RH[1], boot_RH[,1], conf = 0.99)
CI_RH


# SMALL MAMMAL VS RED FOX 
overlap_SV <- overlapEst(rodent$solar, vulpes$solar)
overlap_SV # delta 4
boot_SV<-bootEst(rodentboot, foxboot, adjust=c(NA,1,NA))
boot_SV
CI_SV <- bootCI(overlap_SV[2], boot_SV[,2], conf = 0.99)
CI_SV

# SMALL MAMMAL VS BADGER
overlap_SM <- overlapEst(rodent$solar, meles$solar)
overlap_SM # delta 1
boot_SM<-bootEst(rodentboot, badgerboot, adjust=c(1,NA,NA))
boot_SM
CI_SM <- bootCI(overlap_SM[1], boot_SM[,1], conf = 0.99)
CI_SM

# SMALL MAMMAL VS GENET
overlap_SG <- overlapEst(rodent$solar, genetta$solar)
overlap_SG # delta 1
boot_SG<-bootEst(rodentboot, genetboot, adjust=c(1,NA,NA))
boot_SG
CI_SG <- bootCI(overlap_SG[1], boot_SG[,1], conf = 0.99)
CI_SG


# SMALL MAMMAL VS MONGOOSE
overlap_SH <- overlapEst(rodent$solar, herpestes$solar)
overlap_SH # delta 1
boot_SH<-bootEst(rodentboot, mongooseboot, adjust=c(1,NA,NA))
boot_SH
CI_SH <- bootCI(overlap_SH[1], boot_SH[,1], conf = 0.99)
CI_SH

# PARTRIDGE vs FOX

overlap_PV <- overlapEst(perdiz$solar, vulpes$solar)
overlap_PV 
boot_PV<-bootEst(perdizboot, foxboot, adjust=c(1,NA,NA))
boot_PV
CI_PV <- bootCI(overlap_PV[1], boot_PV[,1], conf = 0.99)
CI_PV

# PARTRIDGE vs BADGER

overlap_PM <- overlapEst(perdiz$solar, meles$solar)
overlap_PM 
boot_PM<-bootEst(perdizboot, badgerboot, adjust=c(1,NA,NA))
boot_PM
CI_PM <- bootCI(overlap_PM[1], boot_PM[,1], conf = 0.99)
CI_PM

# PARTRIDGE vs GENET

overlap_PG <- overlapEst(perdiz$solar, genetta$solar)
overlap_PG 
boot_PG<-bootEst(perdizboot, genetboot, adjust=c(1,NA,NA))
boot_PG
CI_PG <- bootCI(overlap_PG[1], boot_PG[,1], conf = 0.99)
CI_PG


# PARTRIDGE vs MONGOOSE

overlap_PH <- overlapEst(perdiz$solar, herpestes$solar)
overlap_PH 
boot_PH<-bootEst(perdizboot, mongooseboot, adjust=c(1,NA,NA))
boot_PH
CI_PH <- bootCI(overlap_PH[1], boot_PH[,1], conf = 0.99)
CI_PH




# The values resulting from the simulations may differ slightly; this is due to the random component of the bootstrapping process.

# EXAMPLE:  Overlapping common genet vs red fox. Dhat 1 and mean of bootstrapping coefficient differ
overlap_GV
mean(boot_GV[,1])

# This difference is the bootstrap bias and needs to be taken into account when calculating the confidence interval.


# # ¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡  CAUTION !!!!!!!!!!!!!!!!!!!!

# The following estimates of the confidence interval are obtained after having extracted the right column from the bootstrap matrix
# CHECK AGAIN THE NUMBER OF DETECTIONS AND CHOOSE THE CORRECT COEFFICIENT BEFORE.

# CONFIDENCE INTERVAL |  Common genet vs Red fox:
CI_GV <- bootCI(overlap_GV[1], boot_GV[,1], conf = 0.99)
CI_GV
# CONFIDENCE INTERVAL |  Common genet vs Egyptian moongose:
CI_GH <- bootCI(overlap_GH[1], boot_GH[,1], conf = 0.99)
CI_GH
# CONFIDENCE INTERVAL |  Common genet vs European badger:
CI_GM <- bootCI(overlap_GM[1], boot_GM[,1], conf = 0.99)
CI_GM

# CONFIDENCE INTERVAL |  Red fox vs Egyptian moongse:
CI_VH <- bootCI(overlap_VH[1], boot_VH[,1], conf = 0.99)
CI_VH

# CONFIDENCE INTERVAL |  Red fox vs European badger:
CI_VM <- bootCI(overlap_VM[1], boot_VM[,1], conf = 0.99)
CI_VM

# CONFIDENCE INTERVAL |  European badger vs Egyptian moongse:
CI_MH <- bootCI(overlap_MH[1], boot_MH[,1], conf = 0.99)
CI_MH # perc corresponds to the 0.5% and 99.5% percentiles for a 99% confidence interval.

# bootstrap values differ from the estimates because of the bootstrap bias. Therefore the raw percentiles produced by perc
# need to be adjusted to account for this bias. The appropriate confidence interval is perc – ( mean(BS) – Dhat) which corresponds 
# to basic0 in the bootCI output

# ¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡  CAUTION !!!!!!!!!!!!!!!!!!!!

# The coefficient of overlapping takes a value in the interval [0,1]. All the confidence interval estimators except perc involve
# additive correction which might result in values outside of this range.  
# bootCIlogit()  can avoid this problem by carrying out the corrections on a logistic scale and back-transforming.

# IF THERE IS UPPER CI VALUES > 1, bootCIlogit should be applied

?bootCIlogit #arguments of this function are the same from bootCI

# CONFIDENCE INTERVAL |  Common genet vs Red fox:
logCI_GV <- bootCIlogit(overlap_GV[1], boot_GV[,1], conf = 0.99)
logCI_GV
# CONFIDENCE INTERVAL |  Common genet vs Egyptian moongose:
logCI_GH <- bootCIlogit(overlap_GH[1], boot_GH[,1], conf = 0.99)
logCI_GH
# CONFIDENCE INTERVAL |  Common genet vs European badger:
logCI_GM <- bootCIlogit(overlap_GM[1], boot_GM[,1], conf = 0.99)
logCI_GM

# CONFIDENCE INTERVAL |  Red fox vs Egyptian moongse:
logCI_VH <- bootCIlogit(overlap_VH[1], boot_VH[,1], conf = 0.99)
logCI_VH

# CONFIDENCE INTERVAL |  Red fox vs European badger:
logCI_VM <- bootCIlogit(overlap_VM[1], boot_VM[,1], conf = 0.99)
logCI_VM

# CONFIDENCE INTERVAL |  European badger vs Egyptian moongse:
logCI_MH <- bootCIlogit(overlap_MH[1], boot_MH[,1], conf = 0.99)
logCI_MH


# The coefficient of overlap is purely descriptive and thus does not provide a threshold value below which two activity patterns 
# might be significantly different. 


###### WATSON WHEELER TEST ####### Are the activity patterns statistically different?

watson.wheeler.test(list(vulpes$solar_circ, genetta$solar_circ)) # red fox vs. common genet

watson.wheeler.test(list(vulpes$solar_circ, herpestes$solar_circ)) # red fox vs egyptian mongoose

watson.wheeler.test(list(vulpes$solar_circ, meles$solar_circ)) # red fox vs. european badger

watson.wheeler.test(list(genetta$solar_circ, herpestes$solar_circ)) # common genet vs egyptian mongoose

watson.wheeler.test(list(genetta$solar_circ, meles$solar_circ)) # common genet vs european badger

watson.wheeler.test(list(herpestes$solar_circ, meles$solar_circ)) # egyptian mongoose vs european badger




###################################################################################################################################




rabbit_col <- subset(rabbit, area=="SABCOL")
rabbit_oji <- subset(rabbit, area=="SABOJI")
rabbit_mar <- subset(rabbit, area=="SABMAR")

densityPlot(rabbit_col$solar, main= "COLONIZACIÓN")
densityPlot(rabbit_oji$solar, main= "OJILLO")
densityPlot(rabbit_mar$solar, main= "MARQUÉS")


# ACTIVITY PATTERNS OF RED FOX: SHOWS PLASTICITY?

vulpes_mar <- subset(vulpes, area=="SABMAR")
vulpes_oji <- subset(vulpes, area=="SABOJI")
vulpes_col <- subset(vulpes, area=="SABCOL")

densityPlot(vulpes_col$solar, main= "COLONIZACIÓN")
densityPlot(vulpes_oji$solar, main= "OJILLO")
densityPlot(vulpes_mar$solar, main= "MARQUÉS")

?watson.wheeler.test
watson.wheeler.test(list(vulpes_col$solar_circ, vulpes_oji$solar_circ))
watson.wheeler.test(list(vulpes_col$solar_circ, vulpes_mar$solar_circ))
watson.wheeler.test(list(vulpes_oji$solar_circ, vulpes_oji$solar_circ))

overlapEst(vulpes_oji$solar, rabbit_oji$solar)
overlapEst(vulpes_mar$solar, rabbit_mar$solar)
overlapEst(vulpes$solar, rabbit$solar)

# ACTIVITY PATTERNS OF EGYPTIAN MONGOOSE: SHOWS PLASTICITY?

herpestes_mar <- subset(herpestes, area=="SABMAR")
herpestes_oji <- subset(herpestes, area=="SABOJI")
herpestes_col <- subset(herpestes, area=="SABCOL")

par(mfrow=c(1,3))


densityPlot(herpestes_col$solar, main= "COLONIZACIÓN")
densityPlot(herpestes_oji$solar, main= "OJILLO")
densityPlot(herpestes_mar$solar, main= "MARQUÉS")


watson.wheeler.test(list(herpestes_col$solar_circ, herpestes_oji$solar_circ))
watson.wheeler.test(list(herpestes_col$solar_circ, herpestes_mar$solar_circ))
watson.wheeler.test(list(herpestes_oji$solar_circ, herpestes_mar$solar_circ))


### PARWISE COMPARATION_ RABBIT VS FOX VS EGYPTIAN MONGOOSE

par(mfrow=c(3,3))

densityPlot(rabbit_col$solar, main= "COLONIZACIÓN",ylim=c(0, 0.13))
densityPlot(rabbit_oji$solar, main= "OJILLO", ylim=c(0, 0.13))
densityPlot(rabbit_mar$solar, main= "MARQUÉS", ylim=c(0, 0.13))

watson.wheeler.test(list(rabbit_oji$solar_circ, rabbit_mar$solar_circ))  


densityPlot(vulpes_col$solar, main= "COLONIZACIÓN", ylim=c(0, 0.13))
densityPlot(vulpes_oji$solar, main= "OJILLO", ylim=c(0, 0.13))
densityPlot(vulpes_mar$solar, main= "MARQUÉS", ylim=c(0, 0.13))

watson.wheeler.test(list(vulpes_col$solar_circ, vulpes_oji$solar_circ))
watson.wheeler.test(list(vulpes_col$solar_circ, vulpes_mar$solar_circ))                  
watson.wheeler.test(list(vulpes_oji$solar_circ, vulpes_mar$solar_circ))  


overlapEst(vulpes_col$solar, rabbit_col$solar) # could not take this one. there is no sufficient detecions for rabbit in SABCOL
overlapEst(vulpes_oji$solar, rabbit_oji$solar)
overlapEst(vulpes_mar$solar, rabbit_mar$solar)

densityPlot(herpestes_col$solar, main= "COLONIZACIÓN", ylim=c(0, 0.13))
densityPlot(herpestes_oji$solar, main= "OJILLO", ylim=c(0, 0.13))
densityPlot(herpestes_mar$solar, main= "MARQUÉS", ylim=c(0, 0.13))

watson.wheeler.test(list(herpestes_col$solar_circ, herpestes_oji$solar_circ))
watson.wheeler.test(list(herpestes_col$solar_circ, herpestes_mar$solar_circ))                  
watson.wheeler.test(list(herpestes_oji$solar_circ, herpestes_mar$solar_circ))                     

overlapEst(vulpes_col$solar, herpestes_col$solar) # could not take this one. there is no sufficient detecions for rabbit in SABCOL
overlapEst(vulpes_oji$solar, herpestes_oji$solar)
overlapEst(vulpes_mar$solar, herpestes_mar$solar)
                    
## DIEL ACTIVITY PATTERNS: DETECTIONS OF EACH MESOCARNIVORE

# RAO TEST: Our null hypothesis says the data is of a uniform distribution, while the alternate states 
# the data shows signs of directionality


rao.spacing.test(vulpes$solar_circ) # red fox does not show directionality
rao.spacing.test(genetta$solar_circ) # common genet shows directionality => nocturnal
rao.spacing.test(meles$solar_circ) # european badger shows directionality => nocturnal
rao.spacing.test(herpestes$solar_circ) # egyptian mongoose show directionality => diurnal


rao.spacing.test(rabbit$solar_circ)

rao.spacing.test(rodent$solar_circ)
rao.spacing.test(perdiz$solar_circ)

overlap_values <- c(overlap_GV[1], overlap_GH[1], overlap_GM[1], overlap_MH[1], overlap_VM[1], overlap_VH[1])
mean(overlap_values)
sd(overlap_values)


# mean and sd values of overlap coefficients for each sp:

overlap_fox <- c(overlap_GV[1],overlap_VM[1], overlap_VH[1])
mean(overlap_fox)
sd(overlap_fox)

overlap_genet <- c(overlap_GV[1], overlap_GH[1], overlap_GM[1])
mean(overlap_genet)
sd(overlap_genet)

overlap_badger <- c(overlap_GM[1], overlap_MH[1], overlap_VM[1])
mean(overlap_badger)
sd(overlap_badger)

overlap_mongoose <- c(overlap_GH[1], overlap_MH[1], overlap_VH[1])
mean(overlap_mongoose)
sd(overlap_mongoose)


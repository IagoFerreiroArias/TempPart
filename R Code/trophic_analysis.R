#######################################################################################################################
#                                                                                                                     #
#                                                       TROPHIC OVERLAPPING                                           #
#                                                                                                                     #
#######################################################################################################################
citation(package = "activity")
citation(package = "overlap")
citation(package="circular")

library(factoextra)
library(FactoMineR)
library(tidyverse)



diet_data <- read.csv("Diet_data.csv", header = T, sep=";", stringsAsFactors = FALSE)

diet_data <- diet_data %>% remove_rownames %>% column_to_rownames(var="predator")
str(diet_data)
class(diet_data)


diet_data$vegetables <- as.numeric(diet_data$vegetables)

# Correspondence analysis
?CA
diet_ca <- CA(diet_data, graph=FALSE)
?fviz_ca_biplot

fviz_ca_biplot(diet_ca, repel = TRUE, col.col= "#424647", col.row= "#20B2AA")




# confidence elipses 
elipses <- ellipseCA(diet_ca, ellipse="row", method="boot", nbsample=500, axes=c(1,2),
                     col.row="#20B2AA", col.col="black", col.col.ell="#13708F", graph.type = "ggplot", ggoptions = TRUE)



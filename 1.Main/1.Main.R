################################################################################
################################### LIBRARIES ################################## 
library(devtools)
install_github("ajcastanedag/LandCoverEntropy")
################################################################################
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}
# Load libraries
ipak(c("sp","sf","raster","tidyverse","rgdal","rayshader", "ggplot2","viridis",
       "ggdark","magrittr", "leaflet","SciViews","crayon","RColorBrewer",
       "classInt","LandCoverEntropy"))
################################################################################
Main_Fo <- "C://Users//Cowboybebop//Documents//EAGLE//0.Documents//RProjects//SoundScape//"
################################################################################
# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))

# Calculate 
EntropyWz <- Entropy(UrbAtl_Pol,"class_2018",500,"Hex",1)

################################################################################
ggplot(EntropyWz) +
  geom_sf(aes(fill = k)) +
  scale_fill_viridis("Entropy",
                     limits=c(min(unique(EntropyWz$ClasNum)),
                                         max(unique(EntropyWz$ClasNum))),
                     breaks = seq(min(unique(EntropyWz$ClasNum)),
                                  max(unique(EntropyWz$ClasNum)),1)) +
  ggtitle("Entropy map Würzburg") +
  theme_bw()
################################################################################
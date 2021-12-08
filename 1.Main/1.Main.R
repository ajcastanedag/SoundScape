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
       "classInt","LandCoverEntropy","ggplotgui"))
################################################################################
Main_Fo <- "C:\\Users\\nilsk_tpyv1v5\\OneDrive\\Desktop\\SoundScape_Git\\SC\\SoundScape"
################################################################################
# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))
WuLoc <- st_read( paste0(Main_Fo, "\\2.SampleData\\SoundSegmentation\\Coordiantes.gpkg"))

# Calculate 
EntropyWz <- Entropy(UrbAtl_Pol,"class_2018",500,"Hex",1)


#########################################

# accessing the location df
POINT <- st_as_sf(data.frame(lon = WuLoc$geom[[2]][1], lat = WuLoc$geom[[2]][2]), coords = c('lon', 'lat'))

#define a buffer area
buf.a <- st_buffer(POINT, 50)


st_crs(buf.a) <- st_crs(UrbAtl_Pol)

CircleArea <- st_intersection(UrbAtl_Pol[,"class_2018"], buf.a)

CircleArea$area <- st_area(CircleArea)

plot(CircleArea)


hist(CircleArea$area)


# Basic piechart
ggplot(CircleArea, aes(x="", y=class_2018, fill=class_2018)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#########################################

WuLoc$geom[[1]][1]

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
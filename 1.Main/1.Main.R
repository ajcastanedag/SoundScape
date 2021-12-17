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
       "classInt","LandCoverEntropy","ggplotgui","ggpubr","ggthemes","ggridges", "ggpomological"))
################################################################################
Main_Fo <- "C:\\Users\\nilsk_tpyv1v5\\OneDrive\\Desktop\\Soundsc_Git\\SoundScape"
################################################################################
# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))
WuLoc <- st_read( paste0(Main_Fo, "\\2.SampleData\\SoundSegmentation\\Coordinates.gpkg"))

# Calculate 
EntropyWz <- Entropy(UrbAtl_Pol,"class_2018",500,"Hex",1)

#############################################################################################
#buffer locations and get areas#

# accessing the location df
POINT <- st_as_sf(data.frame(lon = WuLoc$geom[[1]][1], lat = WuLoc$geom[[1]][2]), coords = c('lon', 'lat'))

#define a buffer area
buf.a <- st_buffer(POINT, 50)

#set crs
st_crs(buf.a) <- st_crs(UrbAtl_Pol)

#intersect with UA classes
CircleArea <- st_intersection(UrbAtl_Pol[,"SumClass"], buf.a)

#calculate areas
CircleArea$area <- st_area(CircleArea)

#pot areas
plot(CircleArea)

#histogram
pl_1 <- ggplot(aes(x=SumClass,y=as.numeric(area), fill= SumClass), data=CircleArea)+
  geom_col() +
  guides(fill="none")+
  labs(x = "Urban Atlas class", y = "Area in km2")+
  scale_fill_viridis_d()+
  theme_gray()

pl_1

# Basic piechart
ggplot(CircleArea, aes(x="", y=as.numeric(area), fill=SumClass)) +
  geom_bar(stat="identity", width=1) +
  labs(x = "UA Class", y = "Area in km2")+
  coord_polar("y", start=0)

#################################################################################
# summarize UA classes


UrbAtl_Pol$SumClass <- NA

for(i in 1:length(UrbAtl_Pol$SumClass)){
  
  if(UrbAtl_Pol$class_2018[i] %in% c("Continuous urban fabric (S.L. : > 80%)",
                                     "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                                     "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                                     "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                                     "Discontinuous very low density urban fabric (S.L. : < 10%)",
                                     "Industrial, commercial, public, military and private units"
                                     )){
    UrbAtl_Pol$SumClass[i] <- 'Residential'
  } else if(UrbAtl_Pol$class_2018[i] %in% c("Railways and associated land","Fast transit roads and associated land",
                                            "Other roads and associated land","Port areas","Airports")){
    UrbAtl_Pol$SumClass[i] <- 'Traffic_Infrastructures'
  } else if(UrbAtl_Pol$class_2018[i] %in% c( "Land without current use","Mineral extraction and dump sites","Construction sites", 
                                             "Water", "Arable land (annual crops)", "Pastures","Isolated structures","Green urban areas",
                                             "Sports and leisure facilities", "Forests","Permanent crops (vineyards, fruit trees, olive groves)", "Herbaceous vegetation associations (natural grassland, moors...)")){
    UrbAtl_Pol$SumClass[i] <- 'Natural'
  
}
}





Residential <- c("Continuous urban fabric (S.L. : > 80%)",
                 "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                 "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                 "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                 "Discontinuous very low density urban fabric (S.L. : < 10%)",
                 "Industrial,commercial, public, military and private units",
                 "Industrial")

Traffic_Infrastructures <- c("Railways and associated land","Fast transit roads and associated land",
                             "Other roads and associated land","Port areas","Airports")

Natural <- c( "Land without current use","Mineral extraction and dump sites","Construction sites", 
              "Water", "Arable land (annual crops)", "Pastures","Isolated structures","Green urban areas",
              "Sports and leisure facilities", "Forests", "Herbaceous vegetation associations (natural grassland, moors...)")





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
###############################################################################
#spatial plots



P1 <- ggplot(CircleArea) +
      geom_sf(aes(fill = SumClass))+
      theme_pomological_fancy()

#histogram
P2 <- ggplot(aes(x=SumClass,y=as.numeric(area), fill= SumClass), data=CircleArea)+
  geom_col() +
  guides(fill="none")+
  labs(x = "Urban Atlas class", y = "Area in km2")+
  scale_fill_viridis_d()+
  theme_pomological_fancy()

#histogram
P3 <- ggplot(aes(x=Val_ID,y=as.numeric(P_V), fill= Val_ID), data=Ringpark_FdF_Raw)+
  geom_col() +
  guides(fill="none")+
  labs(x = "Sound class", y = "Classification Results")+
  scale_fill_viridis_d()+
  theme_pomological_fancy()

P1 + P2 + P3


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
Main_Fo <- "C:\\Users\\COWBOYBEBOP\\Desktop\\SoundScape"
Main_Fo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape"
################################################################################
# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))
WuLoc <- st_read( paste0(Main_Fo, "\\2.SampleData\\SoundSegmentation\\Coordinates.gpkg"))

# Calculate 
library(LandCoverEntropy)
EntropyWz <- Entropy(UrbAtl_Pol,"class_2018",250,"Hex",2)
plot(EntropyWz[,c("class_2018","MFE")])
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
################################ Entropy Plot ##################################
################################################################################
#set theme
t<-theme_void()+
  theme(
    text=element_text(family = "Playfair Display"),
    plot.margin = unit(c(1,1,1,1), "cm"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#222222"),
    plot.subtitle = element_text(hjust = 0.5, color = "#7A7A7A", size = 6),
   # plot.background = element_rect(fill = "#f9f9f9", color = "#f9f9f9"),
    panel.grid.major = element_line(color = "#f9f9f9"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(color = "#7A7A7A"),
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.box = "vertical"
  )  
theme_set(t)


#plot
ggplot(EntropyWz) +
  geom_sf(aes(fill = MFE)) +
  scale_fill_viridis("Entropy",
                     limits=c(min(unique(EntropyWz$MFE)),
                              max(unique(EntropyWz$MFE))),
                     breaks = seq(min(unique(EntropyWz$MFE)),
                                  max(unique(EntropyWz$MFE)),1)) +
  labs(title = "Entropy map Würzburg",
       subtitle = "Based on Urban Atlas data",
       fill = "Entropy")+ 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  theme_set(t)
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


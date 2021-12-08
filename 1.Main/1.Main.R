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

#############################################################################################
#buffer locations and get areas#

# accessing the location df
POINT <- st_as_sf(data.frame(lon = WuLoc$geom[[2]][1], lat = WuLoc$geom[[2]][2]), coords = c('lon', 'lat'))

#define a buffer area
buf.a <- st_buffer(POINT, 50)

#set crs
st_crs(buf.a) <- st_crs(UrbAtl_Pol)

#intersect with UA classes
CircleArea <- st_intersection(UrbAtl_Pol[,"class_2018"], buf.a)

#calculate areas
CircleArea$area <- st_area(CircleArea)

#plot dat shit
plot(CircleArea)


# Basic piechart
ggplot(CircleArea, aes(x="", y=class_2018, fill=class_2018)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#################################################################################
# summarize UA classes

UrbanAtlas_TST <- UrbAtl_Pol[,c(1,5)] 

UrbanAtlas_TST <- UrbAtl_Pol[,c(1,5)] 

UrbanAtlas_TST <- st_set_geometry(UrbanAtlas_TST, NULL)

Residential <- c("Continuous urban fabric (S.L. : > 80%)",
                 "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                 "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                 "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                 "Discontinuous very low density urban fabric (S.L. : < 10%)")

Industrial <- c("Industrial, commercial, public, military and private units", "Land without current use")


Traffic_Infrastructures <- c("Railways and associated land","Fast transit roads and associated land",
                             "Other roads and associated land","Port areas","Airports")

Non_Residential <- c("Mineral extraction and dump sites","Construction sites", "Isolated structures")

Recreational <- c("Green urban areas", "Sports and leisure facilities", "Forests", "Herbaceous vegetation associations (natural grassland, moors...)")

Others <- c("Water", "Arable land (annual crops)", "Pastures")


for(i in 1:length(UrbAtl_Pol$ITEM2012)){
  if(UrbanAtlas_TST$ITEM2012[i] %in% Residential){UrbanAtlas_TST$ITEM2012[i] <- "Residential"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Industrial){UrbanAtlas_TST$ITEM2012[i] <- "Industrial"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Traffic_Infrastructures){UrbanAtlas_TST$ITEM2012[i] <- "Traffic Infrastructures"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Non_Residential){UrbanAtlas_TST$ITEM2012[i] <- "Non Residential"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Recreational){UrbanAtlas_TST$ITEM2012[i] <- "Recreational"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Others){UrbanAtlas_TST$ITEM2012[i] <- "Others"}
  
}

ggboxplot(UrbAtl_Pol, x = "ITEM2012", y = "Mean", 
          color = "ITEM2012", palette = c("#2bb8cf","#68cfa3","#a6eac9","#9653d5","#47418a","#4a7fdf","#43c3f1","#fbee9c","#c30b8d"),
          order = c("Residential","Industrial","Traffic Infrastructures","Non Residential","Recreational",
                    "Others"),
          ylab = "Mean", xlab = "UA Class")+
  theme(axis.text.x = element_text(angle = 90))

ggline(UrbanAtlas_TST, x = "ITEM2012", y = "Mean", 
       add = c("mean_se", "jitter"), 
       order = c("Residential","Industrial","Traffic Infrastructures","Non Residential","Recreational",


UrbanAtlas_TST <- st_set_geometry(UrbanAtlas_TST, NULL)

Residential <- c("Continuous urban fabric (S.L. : > 80%)",
                 "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                 "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                 "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                 "Discontinuous very low density urban fabric (S.L. : < 10%)")

Industrial <- c("Industrial, commercial, public, military and private units", "Land without current use")


Traffic_Infrastructures <- c("Railways and associated land","Fast transit roads and associated land",
                             "Other roads and associated land","Port areas","Airports")

Non_Residential <- c("Mineral extraction and dump sites","Construction sites", "Isolated structures")

Recreational <- c("Green urban areas", "Sports and leisure facilities", "Forests", "Herbaceous vegetation associations (natural grassland, moors...)")

Others <- c("Water", "Arable land (annual crops)", "Pastures")


for(i in 1:length(UrbAtl_Pol$ITEM2012)){
  if(UrbanAtlas_TST$ITEM2012[i] %in% Residential){UrbanAtlas_TST$ITEM2012[i] <- "Residential"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Industrial){UrbanAtlas_TST$ITEM2012[i] <- "Industrial"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Traffic_Infrastructures){UrbanAtlas_TST$ITEM2012[i] <- "Traffic Infrastructures"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Non_Residential){UrbanAtlas_TST$ITEM2012[i] <- "Non Residential"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Recreational){UrbanAtlas_TST$ITEM2012[i] <- "Recreational"}
  if(UrbanAtlas_TST$ITEM2012[i] %in% Others){UrbanAtlas_TST$ITEM2012[i] <- "Others"}
  
}

ggboxplot(UrbAtl_Pol, x = "ITEM2012", y = "Mean", 
          color = "ITEM2012", palette = c("#2bb8cf","#68cfa3","#a6eac9","#9653d5","#47418a","#4a7fdf","#43c3f1","#fbee9c","#c30b8d"),
          order = c("Residential","Industrial","Traffic Infrastructures","Non Residential","Recreational",
                    "Others"),
          ylab = "Mean", xlab = "UA Class")+
  theme(axis.text.x = element_text(angle = 90))

ggline(UrbanAtlas_TST, x = "ITEM2012", y = "Mean", 
       add = c("mean_se", "jitter"), 
       order = c("Residential","Industrial","Traffic Infrastructures","Non Residential","Recreational",
                 
                 
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
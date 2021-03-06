#####################################################################################################################
#####################################################################################################################
#Urban Morphology & Landscape Metrics
#####################################################################################################################
#####################################################################################################################
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}
# Load libraries
ipak(c("sp","sf","skimr","raster","fasterize","landscapemetrics","corrr","purr","landscapetools","mmand","tidyverse","rgdal", "ggplot2","viridis",
       "ggdark","magrittr", "leaflet","RColorBrewer","osmdata","r5r","geobr","here","osmextract",
       "classInt","LandCoverEntropy","ggplotgui","ggpubr","ggthemes","ggridges", "ggpomological", "data.table", "dplyr", "tmap","stars", "rasterVis"))


#load main fo
Main_Fo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape"

ExportFo <- paste0(Main_Fo,"\\4.Results\\")

# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))

# Add numeric class to Urban atlas
Classes <- sort(unique(UrbAtl_Pol$code_2018))

# Add Class ID as field
UrbAtl_Pol$ID_Cls <- sapply(UrbAtl_Pol$code_2018, FUN = function(x) which(Classes %in% x), simplify = T)

# create empty raster
UA_Raster <- raster::raster()

# crs raster
crs(UA_Raster) <- crs(UrbAtl_Pol)

#match extent
extent(UA_Raster) <- extent(UrbAtl_Pol)

# create raster out of the polygon
UA_Raster <- fasterize(UrbAtl_Pol, UA_Raster,field = "ID_Cls")

#plot the raster
levelplot(UA_Raster)

#### not needed ###############################################################################################
# export as tiff
write_stars(UA_Raster, "4.Results\\UrbAtl_Pol_ras.tif")

#read in tif
UrbAtl_Pol_ras <- "4.Results\\UrbAtl_Pol_ras.tif"

#load raster
UA_Raster <- raster(UA_Raster)

###############################################################################################################
# computation on disc in case of large rasterfari
options_landscapemetrics(to_disk = TRUE)

#check feasability of data
check_landscape(UA_Raster)

# landscape raster
show_landscape(UA_Raster, discrete = TRUE)


# calculate all metrics on (?) level
calculate_lsm(UA_Raster)
lsm_tibble <- calculate_lsm(UA_Raster, level = c("class"), type = "aggregation metric", full_name = T)



#####################################################################################################################
################################################ calculate  metrics around buffer area ##############################
#####################################################################################################################
#load locations
WuLoc <- st_read( paste0(Main_Fo, "\\2.SampleData\\SoundSegmentation\\Coordinates.gpkg"))

# accessing the location df
POINT <- st_as_sf(data.frame(lon = WuLoc$geom[[1]][1], lat = WuLoc$geom[[1]][2]), coords = c('lon', 'lat'))


# buffer metrics
circle_all = sample_lsm(UA_Raster,
                        y = POINT,
                        size = 50,
                        level = "patch",
                        type = c("shape metric"),
                        shape = "circle",
                        full_name = T)
circle_all

# look at the results
circle_all_full_names <- dplyr::left_join(x = circle_all,
                                          y = lsm_abbreviations_names, 
                                          by = "metric")


metrics <- calculate_lsm(UA_Raster, level = c("patch","class"),type = "aggregation metric")
show_correlation(data = metrics, method = "pearson")

metrics <- calculate_lsm(UA_Raster, what = c("patch", "class"))
show_correlation(data= circle_all["metric"], method = "pearson")

################################################################################
# NDVI
################################################################################

library("ggpubr")
ggscatter(NSSounds, x = "NDVI", y = "NsProcent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NDVI", ylab = "NSProcent")

res2 <-cor.test(NSSounds$NDVI, NSSounds$SHDI,  method = "spearman")
res2


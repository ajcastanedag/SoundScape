################################################################################
################################### LIBRARIES ################################## 
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
       "classInt"))
################################################################################
################################################################################
Entropy <- function(Data, Field, GridSpace, GridType="Hex", mode=1){
  ##############################################################################
  Data <- Data[,Field]
  AOI <- st_union(Data)
  ##############################################################################
  cat("Creating grid........") 
  # Define type of grid
  if(GridType == "Hex"){
    Grid <- st_make_grid(Data,
                         square = F,
                         cellsize = c(GridSpace, GridSpace)) %>%
      st_sf() %>%
      st_intersection(AOI)
  } else if(GridType == "Sqr"){
    Grid <- st_make_grid(Data,
                         square = T,
                         cellsize = c(GridSpace, GridSpace)) %>% 
      st_sf()%>%
      st_intersection(AOI)
  }
  cat(green("OK\n"))
  ##############################################################################
  # Add grid blocks ID
  Grid$ID <- seq(from=1, to=length(Grid$geometry))
  ##############################################################################
  cat("Intertsecting grid...")
  Data <- st_intersection(Data, Grid)
  cat(green("OK\n"))
  ##############################################################################
  Data$area <- st_area(Data)
  ##############################################################################
  # building the test df as spatial object
  Full_DF <- as(Data, "Spatial") %>% as.data.frame() 
  row.names(Full_DF) <- NULL
  
  # Add needed fields to calculate entropy
  Grid$MFE <- NaN
  Grid$ClasNum <- 1
  Grid$k <- 1
  Grid$Area <- 0
  
  # Create unique list of available IDs
  IDs <- unique(Grid$ID)
  ##############################################################################
  cat("Running entropy analisis...\n")
  pb <- txtProgressBar(min = 0, max = length(IDs), style = 3)
  
  for(i in 1:length(IDs)){
    
    if(length(Full_DF[Full_DF$ID==IDs[i],Field])>1){
      
      Grid$k[i] <- length(unique(Full_DF[Full_DF$ID==IDs[i],Field]))
      Grid$Area[i] <- sum(Full_DF[Full_DF$ID==IDs[i],"area"])
      Grid$ClasNum[i] <- Grid$k[i]
      
      # building a temporal data.frame 
      temporal <- data.frame("Class"=unique(Full_DF[Full_DF$ID==IDs[i],Field]),
                             "ClassArea"=0,
                             "AreaForm"=0, "LnE" = NaN)
      
      # looping through all grid squares
      for(j in 1:Grid$k[i]){
        # calculating the total class area of each polygon
        temporal[j,"ClassArea"] <- sum(Full_DF[Full_DF[,Field] == temporal[j,"Class"] & Full_DF$ID==IDs[i],"area"])
        
        # calculating the total class area of each polygon per grid square by deviding through the whole area
        temporal[j,"AreaForm"] <- temporal[j,"ClassArea"] / Grid$Area[i]
        
        #
        temporal[j,"LnE"] <- ln(temporal[j,"AreaForm"]) * temporal[j,"AreaForm"]
      }
      # applying the Entropy function
      Grid$MFE[i] <- -(sum(temporal$LnE)/ln(Grid$k[i]))
      
      # Clear woekspace
      rm(temporal)
      
    } else{
      Grid$MFE[i] <- 0
    }
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  ##############################################################################
  if(mode == 2){
    cat("Intertsecting grid...")
    Grid <- st_intersection(Grid, Data)
    cat(green("OK\n"))
    return(Grid)
  } else {
    return(Grid)
  }
  
}
################################################################################
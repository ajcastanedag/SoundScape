library(stringr)
library(ggplot2)
library(ggdark)
library(zoo)
library(MASS)
library(viridis)
library(viridisLite)
library(patchwork)
################################################################################
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
################################################################################
LoadFile <- function(path){
  
  # load data file
  Data <- read.table(path)
  
  # Clean data set
  Data <- Data[,c(-2,-4,-6,-8)]
  
  # Rename fields
  names(Data) <- c("Time","Date","Temp","Hum","Ts","Gs","Ns","Hs")
  
  # process string
  Data[c("Ts","Gs","Ns","Hs")] <- lapply(Data[c("Ts","Gs","Ns","Hs")],
                                         function(x) as.numeric(gsub("[^0-9.]",
                                                                     "",
                                                                     x)))
  
  # Format time
  Data$DateTime <- strptime(paste0(Data$Date, " ", Data$Time),
                            format = "%d/%m/%Y %H:%M:%OS") %>% as.POSIXct
  
  # Final data frame
  Data <- Data[,c(9,3,4,5,6,7,8)]
  
  return(Data)
  
}
################################################################################
single <- function(Data, Field){
  plot <- ggplot(Data, aes(x=DateTime, y=Data[,Field])) +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
    geom_density_2d_filled(alpha = 0.8) +
    geom_point(size=0.01, alpha=0.5, color="black") +
    theme(legend.position='none') +
    ylab(Field)
  
  return(plot)
  
}
################################################################################
patch <- function(Data){
  
  P1 <- single(Data,"Ts")
  P2 <- single(Data,"Hs")
  P3 <- single(Data,"Ns")
  P4 <- single(Data,"Gs")
  
  return((P1 / P2 / P3 / P4))
}
################################################################################
ras <- function(Data, Field){
  plot <- ggplot(Data, aes(x=DateTime, y=Data[,Field])) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position='none') +
    scale_fill_distiller(palette= "Spectral") +
    ylab(Field)
  
  return(plot)
  
}
################################################################################
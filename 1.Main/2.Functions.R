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
reduce <- function(Data, Size){
  
  if(length(Data[,1]) %%  Size == 0){
    
    lengthDf <- length(Data[,1])/Size
    
    Data2 <- data.frame(DateTime=rep(NA,lengthDf),
                        Ts=rep(NA,lengthDf),
                        Gs=rep(NA,lengthDf),
                        Ns=rep(NA,lengthDf),
                        Hs=rep(NA,lengthDf))
    
    pb <- txtProgressBar(min = 0, max = lengthDf, style = 3)

    for(i in 1:lengthDf){

      start <- (i*4)-4+i
      end <- (i*4)+i

      Data2$DateTime[i] <- mean(Data$DateTime[(i*4)-4+i:(i*4)+i]) 
      Data2$Ts[i] <- mean(Data$Ts[start:end], na.rm = TRUE)
      Data2$Gs[i] <- mean(Data$Gs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
      Data2$Ns[i] <- mean(Data$Ns[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
      Data2$Hs[i] <- mean(Data$Hs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
      setTxtProgressBar(pb, i)
    }
    
    Data2$DateTime <- as.POSIXct(Data2$DateTime,origin = "1970-01-01")
    
    close(pb)
    return(Data2)
    
  } else(print("Select another size"))
    
}
################################################################################
getVote <- function(Data){
  
  Data2 <- data.frame(DateTime=Data[,1],Val_ID=NA,Val=NA)
  
  lengthDf <- length(Data[,1])
  pb <- txtProgressBar(min = 0, max = lengthDf, style = 3)

  for(i in 1:lengthDf){
    Data2$Val[i] <- max(Data[i,c(-1)])
    Data2$Val_ID[i] <- names(Data[i,c(-1)])[which(Data[i,c(-1)] == Data2$Val[i])]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(Data2)
}
################################################################################
getVoteRaw <- function(Data){
  
  Data2 <- data.frame(DateTime=Data[,1],Val_ID=NA,Val=NA)
  
  lengthDf <- length(Data[,1])
  pb <- txtProgressBar(min = 0, max = lengthDf, style = 3)
  
  for(i in 1:lengthDf){
    Data2$Val[i] <- max(Data[i,c(-1,-2,-3)])
    Data2$Val_ID[i] <- names(Data[i,c(-1,-2,-3)])[which(Data[i,c(-1,-2,-3)] == Data2$Val[i])]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(Data2)
}
################################################################################
CountClassPerc <- function(Data){
  
  Data <- Data %>%
    group_by(Val_ID) %>%
    summarise(P_V = round(n()*100/length(Data[,1]),2))
  
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
ras <- function(Data, Field, Pal){
  plot <- ggplot(Data, aes(x=DateTime, y=Data[,Field])) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position='none') +
    scale_fill_distiller(palette= Pal) +
    ylab(Field)
  
  return(plot)
  
}
################################################################################
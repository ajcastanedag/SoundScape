################################################################################
# Structure
MainFo <- "C:\\Users\\Cowboybebop\\Documents\\EAGLE\\0.Documents\\RProjects\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
Penny <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))
Svenja <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
Ringpark <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark.txt"))
################################################################################
# Using points and single field
Ringpark<-Ringpark[Ringpark$Ns>0.5,]
single(Ringpark,"Ns")
# Using all fields
patch(Ringpark)
################################################################################
# Using raster/tiles and single field
ras(Ringpark,"Ns","Spectral")
################################################################################

Ringpark2 <- data.frame(DateTime=Ringpark[,1],Val_ID=NA,Val=NA)

for(i in 1:length(Ringpark2$DateTime)){
  Ringpark2$Val[i] <- max(Ringpark[i,c(-1,-2,-2)])
  Ringpark2$Val_ID[i] <- names(Ringpark[i,c(-1,-2,-2)])[which(Ringpark[i,c(-1,-2,-2)] == Ringpark2$Val[i])]
}


plot <- ggplot(Ringpark2) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=factor(Val_ID))) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) 

plot

ggsave(filename=paste0(ExportFo,"Stripes.png"), plot, device = "png", dpi = 90, width = 30,height = 3,units = "cm")


################################################################################


#weird 4 loop

length(Data$Time) %% 5 == 0

length(Data$Time)/50

Data2 <- data.frame(Time=rep(NA,6116),Ts=NA,Gs=NA,Ns=NA,Hs=NA,Check=NA)

for(i in 1:6116){
  
  start <- (i*4)-4+i
  end <- (i*4)+i
  
  Data2$Time[i] <- mean(Data$DateTime[(i*4)-4+i:(i*4)+i])
  Data2$Ts[i] <- mean(Data$Ts[start:end], na.rm = TRUE)
  Data2$Gs[i] <- mean(Data$Gs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  Data2$Ns[i] <- mean(Data$Ns[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  Data2$Hs[i] <- mean(Data$Hs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  
}


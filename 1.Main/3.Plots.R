################################################################################
################################################################################
################################################################################
###
### Plotting Interface of SoundScapeR
###
################################################################################
################################################################################
################################################################################
# Structure
MainFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
Penny <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))
Svenja <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
Ringpark <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark.txt"))
Sebastian <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Sebastian.txt"))
################################################################################
# Using points and single field > 0.5 (tryouts and explanations needed?) 
Ringpark<-Ringpark[Ringpark$Ns>0.5,]

#using single fields
single(Ringpark,"Ns")

# Using all fields
patch(Ringpark)
################################################################################
# Using raster/tiles and single field
ras(Ringpark,"Ns","Spectral")
################################################################################

#creating new summrizing DF
Ringpark2 <- data.frame(DateTime=Ringpark[,1],Val_ID=NA,Val=NA)

#loopinh through classification results returning the maxima of the classification values
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
Svenja <- RingpSvenjaark[1:61680,]
length(Svenja$DateTime) %%  10 == 0

length(Svenja$DateTime)/10

Data2 <- data.frame(DateTime=rep(NA,6116),Ts=NA,Gs=NA,Ns=NA,Hs=NA)

for(i in 1:6116){
  
  start <- (i*4)-4+i
  end <- (i*4)+i
  
  Data2$DateTime[i] <- mean(Svenja$DateTime[(i*4)-4+i:(i*4)+i])
  Data2$Ts[i] <- mean(Svenja$Ts[start:end], na.rm = TRUE)
  Data2$Gs[i] <- mean(Svenja$Gs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  Data2$Ns[i] <- mean(Svenja$Ns[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  Data2$Hs[i] <- mean(Svenja$Hs[(i*4)-4+i:(i*4)+i], na.rm = TRUE)
  
}


Data2_f <- data.frame(DateTime=Data2[,1],Val_ID=NA,Val=NA)

for(i in 1:length(Data2$DateTime)){
  Data2_f$Val[i] <- max(Data2[i,c(-1,-2,-2)])
  Data2_f$Val_ID[i] <- names(Data2[i,c(-1,-2,-2)])[which(Data2[i,c(-1,-2,-2)] == Data2_f$Val[i])]
}
################################################################################
ggplot(Data2_f) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=factor(Val_ID))) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) 

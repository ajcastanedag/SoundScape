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
# Using points and single field
single(Svenja,"Ts")

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
Svenja_R <- reduce(Svenja[1:61160,],10)

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
Svenja_V <- getVote(Svenja_R)
################################################################################
plot <- ggplot(Svenja_V) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=Color)) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  scale_color_manual(values = c('#722b00ff','#e49e00ff','#376111ff','#282828ff'))
   

plot
################################################################################
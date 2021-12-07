################################################################################
# Structure
MainFo <- "C:\\Users\\Cowboybebop\\Documents\\EAGLE\\0.Documents\\RProjects\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
Penny <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))
Svenja <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
################################################################################
# Using points and single field
single(Svenja,"Ts")
# Using all fields
patch(Svenja)
################################################################################
# Using raster/tiles and single field
ras(Svenja,"Ts","Spectral")
################################################################################

Svenja2 <- data.frame(DateTime=Svenja[,1],Val_ID=NA,Val=NA)

for(i in 1:length(Svenja2$DateTime)){
  Svenja2$Val[i] <- max(Svenja[i,c(-1,-2,-2)])
  Svenja2$Val_ID[i] <- names(Svenja[i,c(-1,-2,-2)])[which(Svenja[i,c(-1,-2,-2)] == Svenja2$Val[i])]
}


plot <- ggplot(Svenja2) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=factor(Val_ID),
                         alpha=Val)) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) 

plot

ggsave(filename=paste0(ExportFo,"Stripes.png"), plot, device = "png", dpi = 90, width = 30,height = 3,units = "cm")


################################################################################

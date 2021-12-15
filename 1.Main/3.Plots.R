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
Loewenbruecke <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_LoewenB.txt"))
Jenseits <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Jenseits.txt"))
Markt <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Markt.txt"))
Nicola_Park <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Nicola.txt"))
Ringpark_See <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark_See.txt"))
Main_Kuh <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Main_Kuh.txt"))
################################################################################
# Using points and single field
single(Main_Kuh[Main_Kuh$Ts>0.4,],"Ts")#Ts,Ns,Gs,Hs
# Using all fields
patch(Main_Kuh)
################################################################################
# Using raster/tiles and single field
ras(Penny[Penny$Gs>0.4,],"Gs","Spectral")
################################################################################

#creating new summrizing DF
Ringpark2 <- data.frame(DateTime=Ringpark[,1],Val_ID=NA,Val=NA)

#loopinh through classification results returning the maxima of the classification values
for(i in 1:length(Ringpark2$DateTime)){
  Ringpark2$Val[i] <- max(Ringpark[i,c(-1,-2,-2)])
  Ringpark2$Val_ID[i] <- names(Ringpark[i,c(-1,-2,-2)])[which(Ringpark[i,c(-1,-2,-2)] == Ringpark2$Val[i])]
}

#plotting of the classification maximas
plot <- ggplot(Ringpark2) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=factor(Val_ID))) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  geom_point(aes(x=DateTime,y=Val), size=0.05, alpha=0.1)+
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1)))+
  theme_minimal()

plot

#saving png
ggsave(filename=paste0(ExportFo,"Stripes_Ringpark.png"), plot, device = "png", dpi = 90, width = 30,height = 3,units = "cm")


# Make a summary of the data based on a window size (10 in this case), note that 
# if length(Data) %% size != 0 it wont run so you can crop it manually by indexing
Svenja_R <- reduce(Svenja[1:61160,],10)

################################################################################
# Function to get the high segmented sound per DateTime creating two fields, one 
# with the highest value and the other one with the class that corresponds to that
# value
Svenja_V <- getVote(Svenja_R)
################################################################################
# Make a data frame that counts the frequency of available classes and transform
# it into %. The used data frame in this function is the summericed one, not the
# raw one
Svenja_FdF <- CountClassPerc(Svenja_V)
################################################################################
# Make a data frame that counts the frequency of available classes and transform
# it into % using the raw data set to compare the impact of the summary methodology
Svenja_FdF_Raw <- getVoteRaw(Svenja) %>% CountClassPerc()
################################################################################
# Plot that shit
plot <- ggplot(Svenja_V) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=as.factor(Val_ID))) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  scale_color_manual(values = c('#722b00ff','#e49e00ff','#376111ff','#282828ff'))
   

plot
################################################################################
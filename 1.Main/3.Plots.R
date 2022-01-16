################################################################################
# Structure
MainFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape"
MainFo <- "C:\\Users\\COWBOYBEBOP\\Desktop\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))
################################################################################
L1 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark.txt"))
L2 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Nicola.txt"))
L3 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark_See.txt"))
L4 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))
L5 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
L6 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_LoewenB.txt"))
L7 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Jenseits.txt"))
L8 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Markt.txt"))
L9 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Main_Kuh.txt"))
TestData01A <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Test01SA.TXT"))
TestData01B <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Test01SB.TXT"))
L10 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_TextorStr.txt"))
L11 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_RingP_B.txt"))
L12 <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Sebastian.txt"))
################################################################################
# Using points and single field
single(TestData01A[TestData01A$Gs>0.4,],"Ts")#Ts,Ns,Gs,Hs

# Using all fields
patch(TestData01A)
################################################################################
# Using raster/tiles and single field
ras(Penny[Penny$Gs>0.4,],"Gs","Spectral")
################################################################################

#creating new summrizing DF
Penny2 <- data.frame(DateTime=TestData01B[,1],Val_ID=NA,Val=NA)

#loopinh through classification results returning the maxima of the classification values
for(i in 1:length(Penny2$DateTime)){
  Penny2$Val[i] <- max(TestData01B[i,c(-1,-2,-2)])
  Penny2$Val_ID[i] <- names(TestData01B[i,c(-1,-2,-2)])[which(TestData01B[i,c(-1,-2,-2)] == Penny2$Val[i])]
}

#plotting of the classification maximas
plot <- ggplot(Penny2) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=factor(Val_ID))) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  scale_x_datetime(expand = c(0, 0),date_breaks = "2 hour",
                   date_labels = "%H:%M %p") +
  geom_point(aes(x=DateTime,y=Val), size=0.05, alpha=0.1)+
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1), title = "Classification max"))+
  labs(x = "Time", y = "Classification intensity")+
  theme_grey()

plot


#saving png
ggsave(filename=paste0(ExportFo,"Stripes_TestB.png"), plot, device = "png", dpi = 90, width = 30,height = 20,units = "cm")


# Make a summary of the data based on a window size (10 in this case), note that 
# if length(Data) %% size != 0 it wont run so you can crop it manually by indexing
L1_R <- reduce(L1[1:61544,],8)

################################################################################
# Function to get the high segmented sound per DateTime creating two fields, one 
# with the highest value and the other one with the class that corresponds to that
# value
L1_V <- getVote(L1_R)
################################################################################
# Make a data frame that counts the frequency of available classes and transform
# it into %. The used data frame in this function is the summericed one, not the
# raw one
L1_FdF <- CountClassPerc(L1_V)
################################################################################
# Make a data frame that counts the frequency of available classes and transform
L12_FdF_Raw <- getVoteRaw(L12) %>% CountClassPerc()
################################################################################
# Plot that shit
plot2 <- ggplot(Ringpark_See_V) +
  geom_point(aes(x=DateTime,y=Val), size=0.01, alpha=0.1) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=as.factor(Val_ID))) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1), title = "Classification max"))+
  labs(x = "Time", y = "Classification intensity")+
  scale_color_manual(values = c("Gs"= '#722b00ff',"Hs"='#e49e00ff',"Ns"='#376111ff',"Ts"='#9f7257ff'))+
  theme_gray()#722b00ff, e49e00ff+

   

plot2

#saving png
ggsave(filename=paste0(ExportFo,"Stripes_windowed_Ringpark_See.png"), plot2, device = "png", dpi = 90, width = 30,height = 20,units = "cm")
################################################################################

library(tidyr)
# Filter only data (no T or H)
Svenja <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
Svenja$Sum <- Svenja$Ts + Svenja$Gs + Svenja$Bs + Svenja$Hs 

Svenja2 <- reduce(Svenja[,c(-2,-3)],2)
Svenja2$Sum <- Svenja2$Ts + Svenja2$Gs + Svenja2$Bs + Svenja2$Hs 

SvenjaF <- gather(Svenja, "Class", "Value", 2:5)

# stacked area chart
ggplot(SvenjaF, aes(x=DateTime, y=Value, fill=Class)) + 
  geom_area() +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) 



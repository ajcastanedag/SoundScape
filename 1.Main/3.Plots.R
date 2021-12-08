################################################################################
# Structure
MainFo <- "C:\\Users\\nilsk_tpyv1v5\\OneDrive\\Desktop\\SoundScape_Git\\SC\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))
################################################################################
Penny <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))
Svenja <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Svenja.txt"))
Ringpark <- LoadFile(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Ringpark.txt"))
################################################################################
# Using points and single field
single(Svenja,"Ts")
# Using all fields
patch(Ringpark)
################################################################################
# Using raster/tiles and single field
ras(Ringpark,"Ns","Spectral")
################################################################################
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
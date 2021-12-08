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
single(Svenja,"Ts")
# Using all fields
patch(Ringpark)
################################################################################
# Using raster/tiles and single field
ras(Ringpark,"Ns","Spectral")
################################################################################
Svenja_R <- reduce(Svenja[1:61160,],10)
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
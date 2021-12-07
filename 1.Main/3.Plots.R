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
ras(Svenja,"Ts")
################################################################################


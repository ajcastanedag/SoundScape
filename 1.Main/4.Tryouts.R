################################################################################
# Structure
MainFo <- "C: \\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\2.SampleData\\SoundSegmentation"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
location <- st_read("C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\2.SampleData\\SoundSegmentation\\Coordiantes.gpkg")

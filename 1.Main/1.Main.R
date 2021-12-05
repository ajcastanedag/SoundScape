################################################################################
Main_Fo <- "C://Users//Cowboybebop//Documents//EAGLE//0.Documents//RProjects//SoundScape//"
################################################################################
source(paste0(Main_Fo,"1.Main//2.Functions.R"))
################################################################################
# Load Data (GPKG)
UrbAtl_Pol <- st_read( paste0(Main_Fo, "\\2.SampleData\\LandCover\\Wurzburg_UA_UC.gpkg"))

# Calculate 
EntropyWz <- Entropy(UrbAtl_Pol,"class_2018",500,"Hex",1)

################################################################################
ggplot(EntropyWz3[,1:100]) +
  geom_sf(aes(fill = MFE)) +
  scale_fill_viridis("Entropy", limits=c(0.00,1.00), breaks = seq(0.00,1.00,0.25), labels = round(seq(0,1,0.25),2)) +
  ggtitle("Entropy map Würzburg") +
  theme_bw()
################################################################################
ggplot(EntropyWz) +
  geom_sf(aes(fill = k)) +
  scale_fill_viridis("Entropy",
                     limits=c(min(unique(EntropyWz$ClasNum)),
                                         max(unique(EntropyWz$ClasNum))),
                     breaks = seq(min(unique(EntropyWz$ClasNum)),
                                  max(unique(EntropyWz$ClasNum)),1)) +
  ggtitle("Entropy map Würzburg") +
  theme_bw()
################################################################################

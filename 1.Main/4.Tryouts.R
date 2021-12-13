################################################################################
# Structure
MainFo <- "C: \\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\2.SampleData\\SoundSegmentation"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
location <- st_read("C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\2.SampleData\\SoundSegmentation\\Coordiantes.gpkg")

#spider plot try-outs ( could not wait till christmas, might need help^^)

# install.packages("fmsb")

install.packages("fmsb")

library(fmsb)

Marktplatz1 <- Marktplatz[,4:7]

Marktplatz2 <- rbind(rep(1,1) , rep(0,1) , Marktplatz1)


radarchart(Marktplatz1)

# Fill colors
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(Marktplatz2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2, 
           centerzero = 4,# Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas  


### plotting tryouts 

library(dplyr)
library(ggplot2)
library(stringr)

single1 <- function(Data, Field){
  plot <- ggplot(Data, aes(x=DateTime, y=Data[,Field])) +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
    geom_density_2d_filled(alpha = 0.8) +
    geom_point(size=0.01, alpha=0.5, color="gray12") +
    labs(x = "Time", y = "Classification intensity")+
    theme(legend.position='none') +
    ylab(Field)
  
  return(plot)
  
}

single1(Svenja,"Ts")
### streifen plot tryouts
plot1 <- ggplot(Svenja_V) +
  geom_point(aes(x=DateTime,y=Val), size=0.5, alpha=0.5) +
  geom_vline(mapping=aes(xintercept=DateTime,
                         color=as.factor(Val_ID))) +
  labs(title = "Classification results", x = "Time", y = "Classification %", color = "Classes") +
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  scale_color_manual(values = c('#722b00ff','#e49e00ff','#376111ff','#282828ff'))+
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1)))+
  theme_minimal()


plot1




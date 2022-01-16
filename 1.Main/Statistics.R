#results folder with csvs
ResultsFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\4.Results"

#libs
library("ggpubr")


#data

BSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/BSSounds.csv")
GSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/GSSounds.csv")
HSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/HSSounds.csv")
TSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/TSSounds.csv")


#pearson
ggscatter(GSSounds, x = "NDVI", y = "Sound_share", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
          #xlab = "NDVI", ylab = "Sound share")



res2 <-cor.test(GSSounds$BD, GSSounds$Sound_share,  method = "spearman")
res2



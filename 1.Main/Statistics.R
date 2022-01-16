#results folder with csvs
ResultsFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\4.Results"

#libs
library(ggpubr)
library(corrplot)


#data

BSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/BSSounds.csv")
GSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/GSSounds.csv")
HSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/HSSounds.csv")
TSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/TSSounds.csv")

#test for cor for all var
GSSounds2 <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/GSSounds2.csv")

#pearson
ggscatter(GSSounds, x = "NDVI", y = "Sound_share", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
          #xlab = "NDVI", ylab = "Sound share")


# correlation for all variables
round(cor(GSSounds2),
      digits = 2 # rounded to 2 decimals
)

# improved correlation matrix
corrplot(cor(GSSounds2),
         method = "number",
         type = "upper" # show only upper side
)


#corr for only two var
res2 <-cor.test(GSSounds$BD, GSSounds$Sound_share,  method = "spearman")
res2

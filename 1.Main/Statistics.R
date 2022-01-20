#results folder with csv's
ResultsFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\4.Results"

#libs
library(ggpubr)
library(corrplot)


#data
BSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/BSSounds.csv")
GSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/GSSounds.csv")
HSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/HSSounds.csv")
TSSounds <- read.csv2("C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/TSSounds.csv")


#variables ensemble
EnsembleDF <- BSSounds



#cleaning
GSSounds <- GSSounds[-c(13:15), ]
HSSounds <- HSSounds[-c(13:15), ]
TSSounds <- TSSounds[-c(13:15), ]

EnsembleDF$BS_share <- BSSounds[,2]
EnsembleDF$GS_share <- GSSounds[,2]
EnsembleDF$HS_share <- HSSounds[,2]
EnsembleDF$TS_share <- TSSounds[,2]

#drop share
EnsembleDF$Sound_share <- NULL
EnsembleDF$ï..Location <- NULL
EnsembleDF

#reorder by column
EnsembleDF <- EnsembleDF[, c(13,12,11,10,1,2,3,4,5,6,7,8,9)]
EnsembleDF


#pearson
ggscatter(GSSounds, x = "BD", y = "Sound_share", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
          #xlab = "NDVI", ylab = "Sound share")


# correlation for all variables
round(cor(EnsembleDF),
      digits = 2 # rounded to 2 decimals
)

# improved correlation matrix
corrplot(cor(EnsembleDF),
         method = "number",
         type = "upper" # show only upper side
)


#corr for only two var
res2 <-cor.test(GSSounds$BD, GSSounds$Sound_share,  method = "spearman")
res2

install.packages("performance")
install.packages("see")
library(performance)
library(see)

mode2 <- lm(BS_share ~ LSI + LPI + PD,
             data = EnsembleDF
)

summary(mode2)


check_model(model2)

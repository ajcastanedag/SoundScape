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

#cleaning
GSSounds <- GSSounds[-c(13:15), ]
HSSounds <- HSSounds[-c(13:15), ]
TSSounds <- TSSounds[-c(13:15), ]

#variables ensemble
EnsembleDF <- GSSounds
names(EnsembleDF) <- c("Station","HS_Share","NDVI","BD","DB","RD","DR","LPI","LSI","PD","SHDI")

EnsembleDF$BS_Share <- BSSounds$Sound_share
EnsembleDF$TS_Share <- TSSounds$Sound_share
EnsembleDF$HS_Share <- HSSounds$Sounds_share
EnsembleDF$GS_Share <- GSSounds$Sound_share

#sort dat shit
EnsembleDF <- EnsembleDF[, c(14,13,12,11,2,3,4,5,6,7,8,9,10)]

ggplot(EnsembleDF, aes(x=GS_Share, y=BD))+
  geom_point()



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


#multiple regression
install.packages("performance")
install.packages("see")
library(performance)
library(see)

mode3 <- lm(BS_share ~ NDVI + DR + PD + BD + DB + SHDI,
             data = EnsembleDF
)

summary(mode3)

check_model(mode3)

#multiple variable plot
ggplot(EnsembleDF) +
  aes(x = NDVI, y = BS_share, colour = DR, size = PD) +
  geom_point() +
  scale_color_gradient() +
  labs(
    y = "BS_Share",
    x = "NDVI",
    color = "DR",
    size = "PD"
  ) +
  theme_minimal()

write.csv2(EnsembleDF,"C:/Users/nilsk/Desktop/Soundscape_Git/SoundScape/4.Results/EnsembleDF.csv")

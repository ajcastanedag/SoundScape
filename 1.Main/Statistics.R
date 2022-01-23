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

#Ensemble variables only
EnsembleDFVar <- EnsembleDF[, c(5,6,7,8,9,10,11,12,13)]

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
corrplot(cor(EnsembleDFVar),
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

mode4 <- lm(TS_Share ~ NDVI + DR +RD + BD + PD + SHDI,
             data = EnsembleDF
)

summary(mode4)

check_model(mode4)

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

library(ggstatsplot)

ggcoefstats(mode4)


#anova
boxplot(EnsembleDF$TS_Share~EnsembleDF$GS_Share, 
        main="Boxplot comparing Mileage of Four Brands of Tyre", 
        col= rainbow(4), 
        horizontal = TRUE)

ggplot(EnsembleDF, aes(reorder(Station,BD),NDVI,fill=Station))+
  # ggplot(tyre, aes(Brands,Mileage,fill=Brands))+ # if you want to leave them alphabetic
  geom_jitter(colour = "dark gray",width=.1) +
  stat_boxplot(geom ='errorbar',width = 0.4) +
  geom_boxplot()+
  labs(title="SEM Plot", 
       x = "Stations (sorted)",
       y = "NDVI (in thousands)",
       subtitle ="Gray dots=sample data points, Black dot=outlier, Blue dot=mean, Red=99% confidence interval",
       caption = "Data from https://datascienceplus.com/one-way-anova-in-r/") +
  guides(fill=FALSE) +
  stat_summary(fun.data = "mean_cl_normal", colour = "red", size = 1.5, fun.args = list(conf.int=.99)) +
  stat_summary(geom="point", fun.y=mean, color="blue") +
  theme_bw()

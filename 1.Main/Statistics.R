
ResultsFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\4.Results"
GSSounds <- read.csv(file = '4.Results/GSSounds.csv')
BSSounds <- read_csv(file = "4.Results/BSSounds.csv", sep = ';')
HSSounds <- read_csv(file = "4.Results/HSSounds.csv")
BSSounds <- read_csv(file = "4.Results/BSSounds.csv")

library("ggpubr")
ggscatter(GSSounds, x = "BD", y = "Sound_share", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
          #xlab = "NDVI", ylab = "Sound share")

res2 <-cor.test(GSSounds$BD, GSSounds$Sound_share,  method = "spearman")
res2



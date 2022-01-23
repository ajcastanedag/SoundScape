################################################################################
MainFo <- "C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape"

# Load functions file
source(paste0(MainFo,"\\1.Main\\2.Functions.R"))

################################################################################
#install.packages('gmodels')
library(gmodels)
#install.packages('caret')
library(caret)
################################################################################ TEST A MODEL
# Load Data
TestA <- LoadFile(paste0(MainFo,"\\2.SampleData\\StudioTest\\TESTOS_A.txt")) %>%
  dplyr::select(-Temp,-Hum) 

# Claculate total time of experiment
TestA$DateTime[length(TestA$DateTime)] - TestA$DateTime[1]

# Assign the Classified field and Value
TestA <- TestA %>%
  tibble::add_column(Predicted = colnames(TestA[,2:5])[apply(TestA[,2:5],
                                                             1,
                                                             which.max)],
                     PredictedVal = apply(TestA[, 2:5], 1, max),
                     Reference = rep(c(rep("Ts",300),
                                       rep("Bs",300),
                                       rep("Gs",300),
                                       rep("Hs",300)),
                                     3)[1:3540])

#plotting of the classification 
plotA <- ggplot(TestA) +
  geom_point(mapping=aes(x=DateTime,y=PredictedVal, color=Predicted)) +
  scale_color_manual(values = c('#376111ff','#9f7257ff','#e49e00ff','#9f2b00ff')) +#e49e00ff
  scale_x_datetime(expand = c(0, 0),date_breaks = "2 hour",
                   date_labels = "%H:%M %p") +
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1), title = "Predicted"))+
  labs(x = "Time", y = "classification intensity")+
  theme_grey()

plotA

################################################################################
#Computes the crosstable calculations
CrossTable(as.factor(TestA$Reference),as.factor(TestA$Predicted))

results <- confusionMatrix(as.factor(TestA$Predicted),
                as.factor(TestA$Reference),
                positive = "True")

write.csv(results,"C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\results01.csv" )

restab01 <- as.table(results)

restable02 <- as.matrix(results,what="overall")

restable03 <- as.matrix(results, what = "classes")


write.csv(restable03,"C:\\Users\\nilsk\\Desktop\\Soundscape_Git\\SoundScape\\results03.csv" )
################################################################################ TEST B MODEL
# Load Data
TestB <- LoadFile(paste0(MainFo,"\\2.SampleData\\StudioTest\\TESTOS_B.txt")) %>%
  dplyr::select(-Temp,-Hum) 

# Claculate total time of experiment
TestB$DateTime[length(TestB$DateTime)] - TestB$DateTime[1]

# Assign the Classified field and Value
TestB <- TestB %>%
  tibble::add_column(Predicted = colnames(TestB[,2:5])[apply(TestB[,2:5],
                                                             1,
                                                             which.max)],
                     PredictedVal = apply(TestB[, 2:5], 1, max),
                     Reference = rep(c(rep("Ts",300),
                                       rep("Bs",300),
                                       rep("Gs",300),
                                       rep("Hs",300)),
                                     3)[1:3540])

#plotting of the classification 
plotB <- ggplot(TestB) +
  geom_point(mapping=aes(x=DateTime,y=PredictedVal, color=Predicted)) +
  scale_color_manual(values = c('#9f7257ff','#e49e00ff','#376111ff','#9f2b00ff')) +
  scale_x_datetime(expand = c(0, 0),date_breaks = "2 hour",
                   date_labels = "%H:%M %p") +
  guides(colour = guide_legend(override.aes = list(size=3,linetype= 1), title = "Predicted"))+
  labs(x = "Time", y = "Classification key")+
  theme_grey()

plotB

################################################################################
#Computes the crosstable calculations
CrossTable(as.factor(TestB$Reference),as.factor(TestB$Predicted))
################################################################################
confusionMatrix(as.factor(TestB$Predicted), as.factor(TestB$Reference), positive = "True")

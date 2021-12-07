library(stringr)
library(ggplot2)
library(ggdark)
library(zoo)

# Structure
MainFo <- "C:\\Users\\Cowboybebop\\Documents\\EAGLE\\0.Documents\\RProjects\\SoundScape"
ExportFo <- paste0(MainFo,"\\4.Results\\")

# load data file
Data <- read.table(paste0(MainFo,"\\2.SampleData\\SoundSegmentation\\DATA_Penny.txt"))

# Clean dataset
Data <- Data[,c(-2,-4,-6,-8)]

# Rename fields
names(Data) <- c("Time","Date","Temp","Hum","Ts","Os","Ns","Hs")

# process string
Data[c("Ts","Os","Ns","Hs")] <- lapply(Data[c("Ts","Os","Ns","Hs")],
                                       function(x) as.numeric(gsub("[^0-9.]",
                                                                   "",
                                                                   x)))

# Format time
Data$DateTime <- strptime(paste0(Data$Date, " ", Data$Time),
                          format = "%d/%m/%Y %H:%M:%OS") %>% as.POSIXct

# Final data frame
Data <- Data[,c(9,3,4,5,6,7,8)]

################################################################################


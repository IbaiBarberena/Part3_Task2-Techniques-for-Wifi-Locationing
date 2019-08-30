pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o)


setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainingData <- readRDS("Data/trainingData.rds")
validationData <- readRDS("Data/validationData.rds")
totalData <- readRDS("Data/totalData.rds")



notused <- c("SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")
trainingData[,notused] <- NULL
validationData[,notused] <- NULL
totalData[,notused] <- NULL

WAPS <- colnames(trainingData[,grep("WAP", names(trainingData))])

order <- c(WAPS, "BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")
order_total <- c(WAPS, "BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE", "source")

trainingData <- trainingData[,order]
validationData <- validationData[,order]
totalData <- totalData[,order_total]

rm(order)
rm(order_total)


### SELECTING THE DATA FOR MODELLING ### There are in total 941 unique points in TrainingData. I will select 9 signs of each point and in the points with less than 9 signs, I will select all of them.

trainingData$uniques <- paste(trainingData$LONGITUDE, trainingData$LATITUDE, trainingData$FLOOR, sep="_")

uniques <- trainingData %>% group_by(uniques) %>% tally()

uniquesless9 <- uniques %>% filter(n < 9)

todelete <- which(!is.na(match(trainingData$uniques, uniquesless9$uniques)))


trainingData_2 <- trainingData[-todelete,]

trainmodel <- trainingData_2 %>% group_by(FLOOR, LONGITUDE, LATITUDE) %>% sample_n(9)
notfrequentsignals <- trainingData[todelete,]
trainmodel <- full_join(trainmodel, notfrequentsignals)

# densitymodel <- melt(trainmodel2[, grep("WAP", names(trainmodel2))])
# ggplot(densitymodel, aes(x=densitymodel$value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(0.000000001, 200)

# plot_ly(trainmodel, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,
# colors = c("#E69F00"), size = 0.01) %>%
# add_markers() %>%
# layout(scene = list(xaxis = list(title = "Longitude"),
# yaxis = list(title = "Latitude"),
# zaxis = list(title = "Floor")))

length(unique(paste(trainmodel$LONGITUDE, trainmodel$LATITUDE, trainmodel$FLOOR)))
anyNA(trainmodel)

trainingData$uniques <- NULL
trainmodel$uniques <- NULL

##### Sampling the trainingData #### Phase 1 and 2
# 
set.seed(123)

## Splitting into Training and Testing



partition <- createDataPartition(trainingData$BUILDINGID, p=0.75, list = FALSE)

trainphase <- trainingData[partition,]
testphase <- trainingData[-partition,]



saveRDS(trainmodel,"Data/trainmodel.rds")
saveRDS(trainphase,"Data/trainphase.rds")
saveRDS(testphase,"Data/testphase.rds")
saveRDS(validationData,"Data/validationData.rds")

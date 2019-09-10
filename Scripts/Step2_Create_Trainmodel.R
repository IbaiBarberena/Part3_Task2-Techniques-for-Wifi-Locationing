pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o)



trainingData <- readRDS("Data/trainingData.rds")
validationData <- readRDS("Data/validationData.rds")



notused <- c("SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")
trainingData[,notused] <- NULL
validationData[,notused] <- NULL

WAPS <- colnames(trainingData[,grep("WAP", names(trainingData))])

order <- c(WAPS, "BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE")
order_total <- c(WAPS, "BUILDINGID", "FLOOR", "LATITUDE", "LONGITUDE", "source")

trainingData <- trainingData[,order]
validationData <- validationData[,order]

rm(order)
rm(order_total)


### SELECTING THE DATA FOR MODELLING ### There are in total 941 unique points in TrainingData. I will select 9 signs of each point and in the points with less than 9 signs, I will select all of them.

trainingData$uniques <- paste(trainingData$FLOOR, trainingData$LONGITUDE, trainingData$LATITUDE, sep="_")

uniques <- trainingData %>% group_by(uniques) %>% tally()

uniquesless9 <- uniques %>% filter(n < 9)

todelete <- which(!is.na(match(trainingData$uniques, uniquesless9$uniques)))

trainingData_2 <- trainingData[-todelete,]

# a <- trainingData_2 %>% group_by(uniques) %>% tally()

# trainmodel <- trainingData_2 %>% group_by(FLOOR, LONGITUDE, LATITUDE) %>% sample_n(9)
trainmodel <- trainingData_2 %>% group_by(uniques) %>% sample_n(9)
notfrequentsignals <- trainingData[todelete,]
trainmodel <- full_join(trainmodel, notfrequentsignals)
b <- trainmodel %>% group_by(FLOOR, LONGITUDE, LATITUDE) %>% tally()

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



#################################### PRUEBAS ########################################

## Filtering Building 1

trainmodel <- as.data.frame(trainmodel)

TrBuild1 <- filter(trainmodel, trainmodel$BUILDINGID == "1")

TrBuild1Fl1 <- filter(TrBuild1, TrBuild1$FLOOR == "1")
TrBuild1Fl0 <- filter(TrBuild1, TrBuild1$FLOOR == "0")
TrBuild1Fl2 <- filter(TrBuild1, TrBuild1$FLOOR == "2")
TrBuild1Fl3 <- filter(TrBuild1, TrBuild1$FLOOR == "3")

length(unique(paste(TrBuild1Fl0$LONGITUDE, TrBuild1Fl0$LATITUDE, TrBuild1Fl0$FLOOR)))#86
length(unique(paste(TrBuild1Fl1$LONGITUDE, TrBuild1Fl1$LATITUDE, TrBuild1Fl1$FLOOR)))#123
length(unique(paste(TrBuild1Fl2$LONGITUDE, TrBuild1Fl2$LATITUDE, TrBuild1Fl2$FLOOR)))#104
length(unique(paste(TrBuild1Fl3$LONGITUDE, TrBuild1Fl3$LATITUDE, TrBuild1Fl3$FLOOR)))#54


validationData <- as.data.frame(validationData)

VaBuild1 <- filter(validationData, validationData$BUILDINGID == "1")

VaBuild1Fl1 <- filter(VaBuild1, VaBuild1$FLOOR == "1")
VaBuild1Fl0 <- filter(VaBuild1, VaBuild1$FLOOR == "0")
VaBuild1Fl2 <- filter(VaBuild1, VaBuild1$FLOOR == "2")
VaBuild1Fl3 <- filter(VaBuild1, VaBuild1$FLOOR == "3")

length(unique(paste(VaBuild1Fl0$LONGITUDE, VaBuild1Fl0$LATITUDE, VaBuild1Fl0$FLOOR)))#30
length(unique(paste(VaBuild1Fl1$LONGITUDE, VaBuild1Fl1$LATITUDE, VaBuild1Fl1$FLOOR)))#137
length(unique(paste(VaBuild1Fl2$LONGITUDE, VaBuild1Fl2$LATITUDE, VaBuild1Fl2$FLOOR)))#82
length(unique(paste(VaBuild1Fl3$LONGITUDE, VaBuild1Fl3$LATITUDE, VaBuild1Fl3$FLOOR)))#46



## Prediction 2 out of 5: 
#  Combining trainset and validationset observations.
#  Stratified data: 9 observations per unique location.
#  Cascade approach: 1. Latitude & Longitude 2. Building, 3. Floor
#  Predictive algorithm used: Random Forest for Building and Floor. KNN for Latitude and Longitude
#  Observations from Phone 13 NOT included.

pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN, class)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel <- readRDS("Data/trainmodel.rds")
validationData <- as_tibble(read.csv2("Testing/validationData.csv", sep= ",", stringsAsFactors=FALSE))

validationData <- validationData[,c(1:520)]

anyNA(validationData)
range(validationData)



#### PREPROCESSING ####

validationData <- replace(validationData, validationData == 100, 0)
validationData <- replace(validationData, validationData == -100, 0.001)

for(i in 1:length(grep("WAP", names(validationData)))){
  l <- which(validationData[,i] != 0)
  validationData[l,i] <- validationData[l,i] + 100
}
validationData <- replace(validationData, validationData == 100.001, 0.001)

range(validationData[,c(1:520)])

dmy1 <- dummyVars("~ .", data = trainmodel)
d_trainmodel <- data.frame(predict(dmy1, newdata = trainmodel))

validationData_1 <- validationData[,c(1:520)]
d_validationData_1 <- validationData[,c(1:520)]


#### MODELLING ####

## Finding the best mtry for each model:
WAPS <- grep("WAP", names(trainmodel), value = T)
WAPS_BUILD <- grep("WAP|BUILDING", names(d_trainmodel), value = T)
WAPS_BUILD2 <- grep("WAP|BUILDING", names(trainmodel), value = T)
WAPS_LAT_LONG <- grep("WAP|LATITUDE|LONGITUDE", names(trainmodel), value = T)
WAPS_LAT_LONG_BUILD <- grep("WAP|LATITUDE|LONGITUDE|BUILDING", names(trainmodel), value = T)
WAPS_LAT_LONG_BUILD_FLOOR <- grep("WAP|LATITUDE|LONGITUDE|BUILDING|FLOOR", names(trainmodel), value = T)
WAPS_BUILD_FLOOR <- grep("WAP|BUILDING|FLOOR", names(trainmodel), value = T)
WAPS_BUILD_FLOOR_LATITUDE <- grep("WAP|BUILDING|FLOOR|LATITUDE", names(trainmodel), value = T)
WAPS_LONG_BUILD_FLOOR <- grep("WAP|LONGITUDE|BUILDING|FLOOR", names(trainmodel), value = T)
WAPS_LAT_BUILD_FLOOR <- grep("WAP|LATITUDE|BUILDING|FLOOR", names(trainmodel), value = T)

## PREDICTIONS ##


# Building #
rf2_building <- ranger(BUILDINGID~.,
                       data = trainmodel[,WAPS_BUILD2],
                       mtry = 22)
saveRDS(rf2_building,"Models/Pred2_rf_building.rds")
Pred1_building <- predict(rf2_building, data = validationData_1)


validationData_1$BUILDINGID <- Pred1_building[["predictions"]]
d_validationData_1$BUILDINGID <- Pred1_building[["predictions"]]


dmy2 <- dummyVars("~ .", data = d_validationData_1)
d_validationData_1 <- data.frame(predict(dmy2, newdata = d_validationData_1))

# LATITUDE #

class_knn_latitude2 <- FNN::knn.reg(train = d_trainmodel[,WAPS_BUILD], 
                                    y= trainmodel$LATITUDE,
                                    test= d_validationData_1)

validationData_1$LATITUDE <- class_knn_latitude2[["pred"]]
d_validationData_1$LATITUDE <- class_knn_latitude2[["pred"]]

WAPS_BUILD_LAT <- grep("WAP|BUILDING|LATITUDE", names(d_trainmodel), value = T)

# LONGITUDE #
class_knn_longitude2 <- FNN::knn.reg(train = d_trainmodel[,WAPS_BUILD_LAT], 
                                     y= trainmodel$LONGITUDE,
                                     test= d_validationData_1)

validationData_1$LONGITUDE <- class_knn_longitude2[["pred"]]


# FLOOR #
# mtry_floor <- as.data.frame(tuneRF(x= trainmodel[WAPS_LAT_LONG_BUILD], y= trainmodel$FLOOR, ntree= 100, plot = F))
rf2_floor <- ranger(FLOOR~.,
                    data = trainmodel[,WAPS_LAT_LONG_BUILD_FLOOR],
                    mtry = 44)
saveRDS(rf2_floor,"Models/Pred2_rf_floor.rds")
pred1_floor <- predict(rf2_floor, data = validationData_1)

validationData_1$FLOOR <- pred1_floor[["predictions"]]


# FINAL PREDICTIONS #

Ibai_MIX_casc <- validationData_1[,c(521:524)]
orden <- c("LATITUDE", "LONGITUDE", "FLOOR", "BUILDINGID")
Ibai_MIX_casc <- Ibai_MIX_casc[,orden]
rm(orden)

### If Building 0,1 and Floor 4, replacing them for Floor 3 ###

rows0 <- c()
for(i in 1:nrow(Ibai_MIX_casc)){
  yes0 <- which(Ibai_MIX_casc[i, "BUILDINGID"] == "0" & Ibai_MIX_casc[i, "FLOOR"] == "4")
  if (length(yes0) !=0) {
    rows0 <- c(rows0,i)
  }
}
Ibai_MIX_casc[rows0,"FLOOR"] <- "3"


rows1 <- c()
for(i in 1:nrow(Ibai_MIX_casc)){
  yes1 <- which(Ibai_MIX_casc[i, "BUILDINGID"] == "1" & Ibai_MIX_casc[i, "FLOOR"] == "4")
  if (length(yes1) !=0) {
    rows1 <- c(rows1,i)
  }
}
Ibai_MIX_casc[rows1,"FLOOR"] <- "3"


Ibai_MIX_casc$BUILDINGID <- NULL

Ibai_MIX_casc$FLOOR <- as.factor(Ibai_MIX_casc$FLOOR)

write.csv(Ibai_MIX_casc, file = "Testing/Ibai_MIX_casc.csv", row.names = FALSE, quote = FALSE)


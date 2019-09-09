## Prediction 1 out of 5: 
#  Combining trainset and validationset observations.
#  Stratified data: 9 observations per unique location.
#  Cascade approach: 1. Building, 2. Latitude & Longitude, 3. Floor
#  Predictive algorithm used: Random Forest.
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


#### MODELLING ####

## Finding the best mtry for each model:
WAPS <- grep("WAP", names(trainmodel), value = T)
WAPS_BUILD <- grep("WAP|BUILDING", names(trainmodel), value = T)
WAPS_BUILD_LONG <- grep("WAP|BUILDING|LONGITUDE", names(trainmodel), value = T)
WAPS_BUILD_LAT <- grep("WAP|BUILDING|LATITUDE", names(trainmodel), value = T)
WAPS_BUILD_LONG_LAT_FLOOR <- grep("WAP|BUILDING|LONGITUDE|LATITUDE|FLOOR", names(trainmodel), value = T)

# BUILDING #
rf1_building <- ranger(BUILDINGID~.,
                       data = trainmodel[,WAPS_BUILD],
                       mtry = 11)
saveRDS(rf1_building,"Models/Pred1_rf_building.rds")
# LATITUDE #
rf1_latitude <- ranger(LATITUDE~.,
                       data = trainmodel[,WAPS_BUILD_LAT],
                       mtry = 174)
saveRDS(rf1_latitude,"Models/Pred1_rf_latitude.rds")

# LONGITUDE #
rf1_longitude <- ranger(LONGITUDE~.,
                        data = trainmodel[,WAPS_BUILD_LONG],
                        mtry = 174)
saveRDS(rf1_longitude,"Models/Pred1_rf_longitude.rds")

# FLOOR #
rf1_floor <- ranger(FLOOR~.,
                    data = trainmodel[,WAPS_BUILD_LONG_LAT_FLOOR],
                    mtry = 44)

saveRDS(rf1_floor,"Models/Pred1_rf_longitude.rds")

#### PREDICTIONS #####


# BUILDING #

pred1_building <- predict(rf1_building, data = validationData[WAPS])

validationData_1 <- validationData[WAPS]
validationData_1$BUILDINGID <- pred1_building[["predictions"]]

# LATITUDE #

pred1_latitude <- predict(rf1_latitude, data = validationData_1)

# LONGITUDE #

pred1_longitude <- predict(rf1_longitude, data = validationData_1)

validationData_1$LATITUDE <- pred1_latitude[["predictions"]]
validationData_1$LONGITUDE <- pred1_longitude[["predictions"]]

# FLOOR #
pred1_floor <- predict(rf1_floor, data = validationData_1)
validationData_1$FLOOR <- pred1_floor[["predictions"]]



# FINAL PREDICTIONS #

Ibai_RF_casc <- validationData_1[,c(521:524)]
orden <- c("LATITUDE", "LONGITUDE", "FLOOR", "BUILDINGID")
Ibai_RF_casc <- Ibai_RF_casc[,orden]
rm(orden)

### If Building 0,1 and Floor 4, replacing them for Floor 3 ###

rows0 <- c()
for(i in 1:nrow(Ibai_RF_casc)){
  yes0 <- which(Ibai_RF_casc[i, "BUILDINGID"] == "0" & Ibai_RF_casc[i, "FLOOR"] == "4")
  if (length(yes0) !=0) {
    rows0 <- c(rows0,i)
  }
}
Ibai_RF_casc[rows0,"FLOOR"] <- "3"


rows1 <- c()
for(i in 1:nrow(Ibai_RF_casc)){
  yes1 <- which(Ibai_RF_casc[i, "BUILDINGID"] == "1" & Ibai_RF_casc[i, "FLOOR"] == "4")
  if (length(yes1) !=0) {
    rows1 <- c(rows1,i)
  }
}
Ibai_RF_casc[rows1,"FLOOR"] <- "3"


Ibai_RF_casc$BUILDINGID <- NULL

Ibai_RF_casc$FLOOR <- as.factor(Ibai_RF_casc$FLOOR)

write.csv(Ibai_RF_casc, file = "Testing/Ibai_RF_casc.csv", row.names = FALSE, quote = FALSE)
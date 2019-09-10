## Prediction 3 out of 5: 
#  Combining trainset and validationset observations.
#  Stratified data: 9 observations per unique location.
#  Independent approach: Predicting new columns only by using WAPS
#  Predictive algorithm used: Random Forest for Building and Floor. KNN for Latitude and Longitude
#  Observations from Phone 13 NOT included.

pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN, class)


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
WAPS_BUILD_FLOOR <- grep("WAP|BUILDING|FLOOR", names(trainmodel), value = T)

# BUILDING #
rf3_building <- ranger(BUILDINGID~.,
                       data = trainmodel[,WAPS_BUILD],
                       mtry = 11)
saveRDS(rf3_building,"Models/Pred3_rf_building.rds")

# LATITUDE #
rf3_floor <- ranger(FLOOR~.,
                       data = trainmodel[,WAPS_BUILD_FLOOR],
                       mtry = 44)
saveRDS(rf3_floor,"Models/Pred3_rf_floor.rds")
#### PREDICTIONS #####


# BUILDING #

pred1_building <- predict(rf3_building, data = validationData[WAPS])

validationData_1 <- validationData[WAPS]
validationData_1$BUILDINGID <- pred1_building[["predictions"]]

# FLOOR #
pred1_floor <- predict(rf3_floor, data = validationData_1)
validationData_1$FLOOR <- pred1_floor[["predictions"]]

# LATITUDE #
class_knn_latitude3 <- FNN::knn.reg(train = trainmodel[, WAPS], 
                                   y= trainmodel$LATITUDE,
                                   test= validationData_1[,WAPS])

# LONGITUDE #

class_knn_longitude3 <- FNN::knn.reg(train = trainmodel[, WAPS], 
                                    y= trainmodel$LONGITUDE,
                                    test= validationData_1[,WAPS])


validationData_1$LATITUDE <- class_knn_latitude3[["pred"]]
validationData_1$LONGITUDE <- class_knn_longitude3[["pred"]]

# FINAL PREDICTIONS #

Ibai_MIX_indep <- validationData_1[,c(521:524)]
orden <- c("LATITUDE", "LONGITUDE", "FLOOR", "BUILDINGID")
Ibai_MIX_indep <- Ibai_MIX_indep[,orden]
rm(orden)

### If Building 0,1 and Floor 4, replacing them for Floor 3 ###

rows0 <- c()
for(i in 1:nrow(Ibai_MIX_indep)){
  yes0 <- which(Ibai_MIX_indep[i, "BUILDINGID"] == "0" & Ibai_MIX_indep[i, "FLOOR"] == "4")
  if (length(yes0) !=0) {
    rows0 <- c(rows0,i)
  }
}
Ibai_MIX_indep[rows0,"FLOOR"] <- "3"


rows1 <- c()
for(i in 1:nrow(Ibai_MIX_indep)){
  yes1 <- which(Ibai_MIX_indep[i, "BUILDINGID"] == "1" & Ibai_MIX_indep[i, "FLOOR"] == "4")
  if (length(yes1) !=0) {
    rows1 <- c(rows1,i)
  }
}
Ibai_MIX_indep[rows1,"FLOOR"] <- "3"


Ibai_MIX_indep$BUILDINGID <- NULL

Ibai_MIX_indep$FLOOR <- as.factor(Ibai_MIX_indep$FLOOR)

write.csv(Ibai_MIX_indep, file = "Testing/Ibai_MIX_indep.csv", row.names = FALSE, quote = FALSE)

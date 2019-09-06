pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN, class)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel <- readRDS("Data/trainmodel.rds")
# validationData <- read.csv2("Testing/validationData.csv")
validationData <- readRDS("Data/validationData.rds")

dmy1 <- dummyVars("~ .", data = trainmodel)

d_trainmodel <- data.frame(predict(dmy1, newdata = trainmodel))

## PREDICTIONS ##

d_validationData_1 <- validationData[, c(1:520)]

## Finding the best mtry for each model:
WAPS <- grep("WAP", names(trainmodel), value = T)
WAPS_BUILD <- grep("WAP|BUILDING", names(d_trainmodel), value = T)
WAPS_BUILD_LAT <- grep("WAP|BUILDING|LATITUDE", names(d_trainmodel), value = T)
WAPS_BUILD_LAT_LONG <- grep("WAP|BUILDING|LATITUDE|LONGITUDE", names(d_trainmodel), value = T)


# BUILDING #
class_knn_building <- FNN::knn(train = d_trainmodel[, WAPS], 
                               cl= trainmodel$BUILDINGID,
                               test= d_validationData_1)

d_validationData_1$BUILDINGID <- class_knn_building

Per_knn_building <- confusionMatrix(class_knn_building,validationData$BUILDINGID)
Per_knn_building

# LATITUDE #
dmy2 <- dummyVars("~ .", data = d_validationData_1)
d_validationData_1 <- data.frame(predict(dmy2, newdata = d_validationData_1))

class_knn_latitude <- FNN::knn.reg(train = d_trainmodel[, WAPS_BUILD], 
                                   y= trainmodel$LATITUDE,
                                   test= d_validationData_1)
Per_knn_latitude <- postResample(class_knn_latitude[["pred"]],validationData$LATITUDE)
Per_knn_latitude

# LONGITUDE #
d_validationData_1$LATITUDE <- class_knn_latitude[["pred"]]

class_knn_longitude <- FNN::knn.reg(train = d_trainmodel[, WAPS_BUILD_LAT], 
                                    y= trainmodel$LONGITUDE,
                                    test= d_validationData_1)
Per_knn_longitude <- postResample(class_knn_longitude[["pred"]],validationData$LONGITUDE)
Per_knn_longitude

# FLOOR #
d_validationData_1$LONGITUDE <- class_knn_longitude[["pred"]]
# dmy3 <- dummyVars("~ .", data = d_validationData_1)
# d_validationData_1 <- data.frame(predict(dmy3, newdata = WAPS_BUILD_LAT_LONG))

RFINKNN_FLOOR <- ranger(FLOOR~.,
                        data = trainmodel[,WAPS_BUILD_LONG_LAT_FLOOR],
                        mtry = 44)



class_knn_floor <- FNN::knn(train = d_trainmodel[, WAPS_BUILD_LAT_LONG], 
                            cl= trainmodel$FLOOR,
                            test= d_validationData_1)
Per_knn_floor <- confusionMatrix(class_knn_floor,validationData$FLOOR)
Per_knn_floor


## GATHERING THE PREDICTIONS ##

Ibai_knn_casc <- data.frame(BUILDINGID = class_knn_building, LATITUDE = class_knn_latitude[["pred"]], LONGITUDE = class_knn_longitude[["pred"]], FLOOR = class_knn_floor)

### If Building 0,1 and Floor 4, replacing them for Floor 3 ###

rows0 <- c()
for(i in 1:nrow(Ibai_knn_casc)){
  yes0 <- which(Ibai_knn_casc[i, "BUILDINGID"] == "0" & Ibai_knn_casc[i, "FLOOR"] == "4")
  if (length(yes0) !=0) {
    rows0 <- c(rows0,i)
  }
}
Ibai_knn_casc[rows0,"FLOOR"] <- "3"


rows1 <- c()
for(i in 1:nrow(Ibai_knn_casc)){
  yes1 <- which(Ibai_knn_casc[i, "BUILDINGID"] == "1" & Ibai_knn_casc[i, "FLOOR"] == "4")
  if (length(yes1) !=0) {
    rows1 <- c(rows1,i)
  }
}
Ibai_knn_casc[rows1,"FLOOR"] <- "3"
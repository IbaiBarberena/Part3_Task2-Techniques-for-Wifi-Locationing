## Modelling: CASCADE
## Without signals between -34 and 0 and -10 and -20
## With user6
## RF

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel2 <- readRDS("Data/trainmodel.rds")
trainphase2 <- readRDS("Data/trainphase.rds")
testphase2 <- readRDS("Data/testphase.rds")
validationData2 <- readRDS("Data/validationData.rds")


##### MODELLING EACH CATEGORY #####

## Finding the best mtry for each model:
WAPS <- colnames(trainmodel2[,grep("WAP", names(trainmodel2))])
WAPS_BUILD <- colnames(trainmodel2[,grep("WAP|BUILDING", names(trainmodel2))])
WAPS_BUILD_FLOOR <- colnames(trainmodel2[,grep("WAP|BUILDING|FLOOR", names(trainmodel2))])
WAPS_BUILD_FLOOR_LAT <- colnames(trainmodel2[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(trainmodel2))])
WAPS_BUILD_FLOOR_LAT_LONG <- colnames(trainmodel2[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(trainmodel2))])


# mtry_building <- as.data.frame(tuneRF(x= trainmodel2[WAPS], y= trainmodel2$BUILDINGID, ntree= 100, plot = F))
# mtry_floor <- as.data.frame(tuneRF(x= trainmodel2[WAPS_BUILD], y= trainmodel2$FLOOR, ntree= 100, plot = F))
# mtry_latitude <- as.data.frame(tuneRF(x= trainmodel2[WAPS_BUILD_FLOOR], y= trainmodel2$LATITUDE, ntree= 100, plot = F))
# mtry_longitude <- as.data.frame(tuneRF(x= trainmodel2[WAPS_BUILD_FLOOR_LAT], y= trainmodel2$LONGITUDE, ntree= 100, plot = F))
# 
# mtry_rf <- list(mtry_building, mtry_floor, mtry_longitude,mtry_latitude)
# 
# mtry_metrix <- c()
# 
# for (i in 1:4){
#   
#   min_mtry <- mtry_rf[[i]][which.min(mtry_rf[[i]][["OOBError"]]),1]
#   results[i] <- cbind(mtry_metrix, min_mtry)
# }
# 
# print(results)

# mtry building: 
# mtry floor: 
# mtry latitude: 
# mtry longitude: 


##### CREATING THE PREDICTIVE MODELS FOR EACH COLUMN ####

rf2_building <- ranger(BUILDINGID~., 
                       data = trainmodel2[,WAPS],
                       mtry = )
saveRDS(rf2_building, "Models/rf1_building.rds")

rf2_floor <- ranger(FLOOR~., 
                    data = trainmodel2[,WAPS_BUILD],
                    mtry = )
saveRDS(rf2_floor, "Models/rf1_floor.rds")

rf2_latitude <- ranger(LATITUDE~., 
                       data = trainmodel2[,WAPS_BUILD_FLOOR],
                       mtry = )
saveRDS(rf2_latitude, "Models/rf1_latitude.rds")

rf2_longitude <- ranger(LATITUDE~., 
                        data = trainmodel2[,WAPS_BUILD_FLOOR],
                        mtry = )
saveRDS(rf2_longitude, "Models/rf1_longitude.rds")

###
rm(WAPS_BUILD, WAPS_BUILD_FLOOR, WAPS_BUILD_FLOOR_LAT, WAPS_BUILD_FLOOR_LAT_LONG, mtry_building, mtry_floor, mtry_longitude, mtry_latitude)
###
rf2_building <- readRDS("Models/rf1_building.rds")
rf2_floor <- readRDS("Models/rf1_building.rds")
rf2_latitude <- readRDS("Models/rf1_building.rds")
rf2_longitude <- readRDS("Models/rf1_building.rds")



#### PREDICTIONS #####

## PHASE 1 ## Measuring the performance in trainphase2

# BUILDING #

mtry_building <- tuneRF(x= trainphase1[1:520], y= trainphase1$FLOOR, ntree= 100, plot = F) # mtry = 88

WAP_FLOOR <- grep("WAP|FLOOR", names(train_sample))

rf_floor <- ranger(FLOOR~., 
                   data = trainphase1[,WAP_FLOOR],
                   mtry = 88)

pred.floor <- predict(rf_floor, data = trainphase1)

CM_rf_floor <- confusionMatrix(pred.floor[["predictions"]],trainphase1$FLOOR)
CM_rf_floor


# FLOOR #
trainphase2_2 <- trainphase2[WAPS]
trainphase2_2$BUILDINGID <- pred2_building[["predictions"]]

pred2_floor <- predict(rf2_floor, data = trainphase2_2)
Per_rf2_floor <- confusionMatrix(pred2_floor[["predictions"]],trainphase2$FLOOR)
Per_rf2_floor

# LATITUDE #
trainphase2_2$FLOOR <- pred2_floor[["predictions"]]

pred2_latitude <- predict(rf2_latitude, data = trainphase2_2)
Per_rf2_latitude <- postResample(pred2_latitude[["predictions"]],trainphase2$LATITUDE)
Per_rf2_latitude

# LONGITUDE #
trainphase2_2$LATITUDE <- pred2_latitude[["predictions"]]

pred2_longitude <- predict(rf2_longitude, data = trainphase2_2)
Per_rf2_longitude <- postResample(pred2_longitude[["predictions"]],trainphase2$LONGITUDE)
Per_rf2_longitude

trainphase2_2$LONGITUDE <- pred2_longitude[["predictions"]]

### PLOTTING THE ERRORS ###


## PHASE 2 ## Measuring the performance in testphase2

# BUILDING #

pred2_building <- predict(rf2_building, data = testphase2[WAPS])
Per_rf2_building <- confusionMatrix(pred2_building[["predictions"]],testphase2$BUILDINGID)
Per_rf2_building

# FLOOR #
testphase2_2 <- testphase2[WAPS]
testphase2_2$BUILDINGID <- pred2_building[["predictions"]]

pred2_floor <- predict(rf2_floor, data = testphase2_2)
Per_rf2_floor <- confusionMatrix(pred2_floor[["predictions"]],testphase2$FLOOR)
Per_rf2_floor

# LATITUDE #
testphase2_2$FLOOR <- pred2_floor[["predictions"]]

pred2_latitude <- predict(rf2_latitude, data = testphase2_2)
Per_rf2_latitude <- postResample(pred2_latitude[["predictions"]],testphase2$LATITUDE)
Per_rf2_latitude

# LONGITUDE #
testphase2_2$LATITUDE <- pred2_latitude[["predictions"]]

pred2_longitude <- predict(rf2_longitude, data = testphase2_2)
Per_rf2_longitude <- postResample(pred2_longitude[["predictions"]],testphase2$LONGITUDE)
Per_rf2_longitude

testphase2_2$LONGITUDE <- pred2_longitude[["predictions"]]

### Plotting the erros ###

## PHASE 3 ## Measuring the performance in validationData2

# BUILDING #

pred2_building <- predict(rf2_building, data = validationData2[WAPS])
Per_rf2_building <- confusionMatrix(pred2_building[["predictions"]],validationData2$BUILDINGID)
Per_rf2_building

# FLOOR #
validationData2_2 <- validationData2[WAPS]
validationData2_2$BUILDINGID <- pred2_building[["predictions"]]

pred2_floor <- predict(rf2_floor, data = validationData2_2)
Per_rf2_floor <- confusionMatrix(pred2_floor[["predictions"]],validationData2$FLOOR)
Per_rf2_floor

# LATITUDE #
validationData2_2$FLOOR <- pred2_floor[["predictions"]]

pred2_latitude <- predict(rf2_latitude, data = validationData2_2)
Per_rf2_latitude <- postResample(pred2_latitude[["predictions"]],validationData2$LATITUDE)
Per_rf2_latitude

# LONGITUDE #
validationData2_2$LATITUDE <- pred2_latitude[["predictions"]]

pred2_longitude <- predict(rf2_longitude, data = validationData2_2)
Per_rf2_longitude <- postResample(pred2_longitude[["predictions"]],validationData2$LONGITUDE)
Per_rf2_longitude

validationData2_2$LONGITUDE <- pred2_longitude[["predictions"]]

### Plotting the erros ###


# Results: 

rm(validationData2_2, trainphase2_2, testphase2_2)

SELECT Table1.User_ID, Table2.order_timestamp
  FROM Table1
    JOIN Table2
      ON Table1.User_ID = Table2.User_ID
        WHERE Table1.order_timestamp = N AND Table2.order_timestamp > N

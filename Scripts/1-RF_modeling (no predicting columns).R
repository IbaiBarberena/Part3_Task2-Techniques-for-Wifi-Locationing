## With user6
## With empty variables
## With signals between -34 and 0 and -10 and -20
## Modelling: no cascade
## RF

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainphase <- readRDS("Data/trainphase.rds")
testphase <- readRDS("Data/testphase.rds")
validationData - readRDS("Data/validationData.rds")


##### CREATING AN OPTIMIZED DATASET IN ORDER TO CREATE THE CATEGORIES #####

##### MODELLING EACH CATEGORY #####

## Finding the best mtry for each model:
# WAPS <- colnames(trainphase[,grep("WAP", names(trainphase))])
# WAPS_BUILD <- colnames(trainphase[,grep("WAP|BUILDING", names(trainphase))])
# WAPS_BUILD_FLOOR <- colnames(trainphase[,grep("WAP|BUILDING|FLOOR", names(trainphase))])
# WAPS_BUILD_FLOOR_LAT <- colnames(trainphase[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(trainphase))])
# WAPS_BUILD_FLOOR_LAT_LONG <- colnames(trainphase[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(trainphase))])


# mtry_building <- as.data.frame(tuneRF(x= trainphase[WAPS], y= trainphase$BUILDINGID, ntree= 100, plot = F))
# mtry_floor <- as.data.frame(tuneRF(x= trainphase[WAPS_BUILD], y= trainphase$FLOOR, ntree= 100, plot = F))
# mtry_latitude <- as.data.frame(tuneRF(x= trainphase[WAPS_BUILD_FLOOR], y= trainphase$LATITUDE, ntree= 100, plot = F))
# mtry_longitude <- as.data.frame(tuneRF(x= trainphase[WAPS_BUILD_FLOOR_LAT], y= trainphase$LONGITUDE, ntree= 100, plot = F))
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

# mtry building: 44
# mtry floor: 88
# mtry latitude: 174
# mtry longitude: 174


##### CREATING THE PREDICTIVE MODELS FOR EACH COLUMN ####

rf1_bulinding <- ranger(BUILDINGID~., 
                        data = trainphase[,WAPS],
                        mtry = )

rf1_floor <- ranger(FLOOR~., 
                    data = trainphase[,WAPS_BUILD],
                    mtry = )

rf1_latitude <- ranger(LATITUDE~., 
                       data = trainphase[,WAPS_BUILD_FLOOR],
                       mtry = )

rf1_longitude <- ranger(LATITUDE~., 
                       data = trainphase[,WAPS_BUILD_FLOOR],
                       mtry = )

###

#### PREDICTIONS #####

## PHASE 1 ## Measuring the performance in trainphase

# BUILDING #

pred1_building <- predict(rf1_bulinding, data = trainphase[WAPS])
Per_rf1_building <- confusionMatrix(pred1_building[["predictions"]],trainphase$BUILDINGID)
Per_rf1_building

# FLOOR #
trainphase_1 <- trainphase[WAPS]
trainphase_1$BUILDINGID <- pred1_building[["predictions"]]

pred1_floor <- predict(rf1_floor, data = trainphase_1)
Per_rf1_floor <- confusionMatrix(pred1_floor[["predictions"]],trainphase$FLOOR)
Per_rf1_floor

# LATITUDE #
trainphase_1$FLOOR <- pred1_floor[["predictions"]]

pred1_latitude <- predict(rf1_latitude, data = trainphase_1)
Per_rf1_latitude <- postResample(pred1_latitude[["predictions"]],trainphase$LATITUDE)
Per_rf1_latitude

# LONGITUDE #
trainphase_1$LATITUDE <- pred1_latitude[["predictions"]]

pred1_longitude <- predict(rf1_longitude, data = trainphase_1)
Per_rf1_longitude <- postResample(pred1_longitude[["predictions"]],trainphase$LONGITUDE)
Per_rf1_longitude

trainphase_1$LONGITUDE <- pred1_longitude[["predictions"]]

### PLOTTING THE ERRORS ###


## PHASE 2 ## Measuring the performance in testphase

# BUILDING #

pred1_building <- predict(rf1_bulinding, data = testphase[WAPS])
Per_rf1_building <- confusionMatrix(pred1_building[["predictions"]],testphase$BUILDINGID)
Per_rf1_building

# FLOOR #
testphase_1 <- testphase[WAPS]
testphase_1$BUILDINGID <- pred1_building[["predictions"]]

pred1_floor <- predict(rf1_floor, data = testphase_1)
Per_rf1_floor <- confusionMatrix(pred1_floor[["predictions"]],testphase$FLOOR)
Per_rf1_floor

# LATITUDE #
testphase_1$FLOOR <- pred1_floor[["predictions"]]

pred1_latitude <- predict(rf1_latitude, data = testphase_1)
Per_rf1_latitude <- postResample(pred1_latitude[["predictions"]],testphase$LATITUDE)
Per_rf1_latitude

# LONGITUDE #
testphase_1$LATITUDE <- pred1_latitude[["predictions"]]

pred1_longitude <- predict(rf1_longitude, data = testphase_1)
Per_rf1_longitude <- postResample(pred1_longitude[["predictions"]],testphase$LONGITUDE)
Per_rf1_longitude

testphase_1$LONGITUDE <- pred1_longitude[["predictions"]]

### Plotting the erros ###

## PHASE 3 ## Measuring the performance in validationData

# BUILDING #

pred1_building <- predict(rf1_bulinding, data = validationData[WAPS])
Per_rf1_building <- confusionMatrix(pred1_building[["predictions"]],validationData$BUILDINGID)
Per_rf1_building

# FLOOR #
validationData_1 <- validationData[WAPS]
validationData_1$BUILDINGID <- pred1_building[["predictions"]]

pred1_floor <- predict(rf1_floor, data = validationData_1)
Per_rf1_floor <- confusionMatrix(pred1_floor[["predictions"]],validationData$FLOOR)
Per_rf1_floor

# LATITUDE #
validationData_1$FLOOR <- pred1_floor[["predictions"]]

pred1_latitude <- predict(rf1_latitude, data = validationData_1)
Per_rf1_latitude <- postResample(pred1_latitude[["predictions"]],validationData$LATITUDE)
Per_rf1_latitude

# LONGITUDE #
validationData_1$LATITUDE <- pred1_latitude[["predictions"]]

pred1_longitude <- predict(rf1_longitude, data = validationData_1)
Per_rf1_longitude <- postResample(pred1_longitude[["predictions"]],validationData$LONGITUDE)
Per_rf1_longitude

validationData_1$LONGITUDE <- pred1_longitude[["predictions"]]

### Plotting the erros ###


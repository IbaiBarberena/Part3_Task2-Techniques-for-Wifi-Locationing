## Without signals between -34 and 0 and -10 and -20
## With user6
## Modelling: no cascade
## RF

pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel <- readRDS("Data/trainmodel.rds")
trainphase <- readRDS("Data/trainphase.rds")
testphase <- readRDS("Data/testphase.rds")
validationData <- readRDS("Data/validationData.rds")

densitymodel <- melt(trainmodel[, grep("WAP", names(trainmodel))])
# ggplot(densitymodel, aes(x=densitymodel$value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(0.000000001, 200)
# range(densitymodel$value)

##### MODELLING EACH CATEGORY #####

## Finding the best mtry for each model:
WAPS <- colnames(trainmodel[,grep("WAP", names(trainmodel))])
WAPS_BUILD <- colnames(trainmodel[,grep("WAP|BUILDING", names(trainmodel))])
WAPS_BUILD_FLOOR <- colnames(trainmodel[,grep("WAP|BUILDING|FLOOR", names(trainmodel))])
WAPS_BUILD_FLOOR_LAT <- colnames(trainmodel[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(trainmodel))])
WAPS_BUILD_FLOOR_LAT_LONG <- colnames(trainmodel[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(trainmodel))])


# mtry_building <- as.data.frame(tuneRF(x= trainmodel[WAPS], y= trainmodel$BUILDINGID, ntree= 100, plot = F))
# mtry_floor <- as.data.frame(tuneRF(x= trainmodel[WAPS_BUILD], y= trainmodel$FLOOR, ntree= 100, plot = F))
# mtry_latitude <- as.data.frame(tuneRF(x= trainmodel[WAPS_BUILD_FLOOR], y= trainmodel$LATITUDE, ntree= 100, plot = F))
# mtry_longitude <- as.data.frame(tuneRF(x= trainmodel[WAPS_BUILD_FLOOR_LAT], y= trainmodel$LONGITUDE, ntree= 100, plot = F))
# 
# mtry_rf <- list(mtry_building, mtry_floor, mtry_longitude,mtry_latitude)

# mtry_metrix <- c()
# results <- c()
# for (i in 1:4){
# 
#   min_mtry <- mtry_rf[[i]][which.min(mtry_rf[[i]][["OOBError"]]),1]
#   results[i] <- cbind(mtry_metrix, min_mtry)
# }
# 
# print(results)

# mtry building: 11
# mtry floor: 44
# mtry latitude: 174
# mtry longitude: 174


##### CREATING THE PREDICTIVE MODELS FOR EACH COLUMN #### RF

fitControl <- trainControl(method = "repeatedcv",
                           number=10,
                           repeats = 1)

# BUILDING #
# rf1_building <- ranger(BUILDINGID~.,
#                         data = trainmodel[,WAPS_BUILD],
#                         mtry = 11)
# saveRDS(rf1_building, "Models/rf1_building.rds")

start_knn1_build <- Sys.time()
knn1_building <- train(y= trainmodel$BUILDINGID,
                       x = trainmodel[,WAPS],
                       method = "knn",
                       trControl = fitControl)
end_knn1_build <- Sys.time()
time_knn1_build <- start_knn1_build - end_knn1_build
saveRDS(knn1_building, "Models/knn1_building")



# FLOOR #
# rf1_floor <- ranger(FLOOR~.,
#                     data = trainmodel[,WAPS_BUILD_FLOOR],
#                     mtry = 44)
# saveRDS(rf1_floor, "Models/rf1_floor.rds")

# start_knn1_floor <- Sys.time()
# knn1_floor <- train(y= trainmodel$FLOOR, x= trainmodel[,WAPS_BUILD_FLOOR], method = "knn", trControl = fitControl)
# end_knn1_floor <- Sys.time()
# time_knn1_floor <- start_knn1_build - end_knn1_build
# saveRDS(knn1_floor, "Models/knn1_floor")

# LATITUDE #
# rf1_latitude <- ranger(LATITUDE~.,
#                        data = trainmodel[,WAPS_BUILD_FLOOR_LAT],
#                        mtry = 174)
# saveRDS(rf1_latitude, "Models/rf1_latitude.rds")

# start_knn1_latitude <- Sys.time()
# knn1_latitude <- train(y= trainmodel$LATITUDE x= trainmodel[,WAPS_BUILD_FLOOR_LAT], method = "knn", trControl = fitControl)
# end_knn1_latitude <- Sys.time()
# time_knn1_latitude <- start_knn1_latitude - end_knn1_latitude
# saveRDS(knn1_latitude, "Models/knn1_latitude")

# LONGITUDE #
# rf1_longitude <- ranger(LONGITUDE~.,
#                        data = trainmodel[,WAPS_BUILD_FLOOR_LAT_LONG],
#                        mtry = 174)
# saveRDS(rf1_longitude, "Models/rf1_longitude.rds")

knn1_longitude <- 

###
rm(WAPS_BUILD, WAPS_BUILD_FLOOR, WAPS_BUILD_FLOOR_LAT, WAPS_BUILD_FLOOR_LAT_LONG, mtry_building, mtry_floor, mtry_longitude, mtry_latitude)
###
rf1_building <- readRDS("Models/rf1_building.rds")
rf1_floor <- readRDS("Models/rf1_floor.rds")
rf1_latitude <- readRDS("Models/rf1_latitude.rds")
rf1_longitude <- readRDS("Models/rf1_longitude.rds")

#### PREDICTIONS #####

## PHASE 1 ## Measuring the performance in trainphase

# BUILDING #

pred1_building <- predict(rf1_building, data = trainphase[WAPS])
Per_rf1_building <- confusionMatrix(pred1_building[["predictions"]],trainphase$BUILDINGID)
Per_rf1_building

pred1_knn_building <- predict(knn1_building, trainphase[WAPS])
Per_knn1_building <- confusionMatrix(pred1_knn_building, trainphase$BUILDINGID)
Per_knn1_building


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

pred1_building <- predict(rf1_building, data = testphase[WAPS])
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

pred1_building <- predict(rf1_building, data = validationData[WAPS])
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


#Classification Error Building

df_error_check_v2 <- gdata::combine(validationData[,c(521:524)], validationData_1[,c(521:524)])
df_error_check_v2$source <- as.character(df_error_check_v2$source)
df_error_check_v2$source[df_error_check_v2$source == "validationData[, c(521:524)]"] <- "real"
df_error_check_v2$source[df_error_check_v2$source == "validationData_1[, c(521:524)]"] <- "predicted"

df_error_check <- cbind(validationData[,c(521:524)], validationData_1[,c(521:524)])
names(df_error_check)[1:4] <- paste("real", names(df_error_check)[1:4], sep = "_")
names(df_error_check)[5:8] <- paste("pred", names(df_error_check)[5:8], sep = "_")



### Plotting the distances ###

plot_ly(df_error_check_v2, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
        colors = c("#14ec14", "#FF0000"), size = 0.01) %>%
  add_markers() %>%
  layout(scene= list(xaxis = list(title = "Longitude"),
                       yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


df_error_check$long_diff = abs(abs(df_error_check$real_LONGITUDE) - abs(df_error_check$pred_LONGITUDE))
df_error_check$lat_diff = abs(abs(df_error_check$real_LATITUDE) - abs(df_error_check$pred_LATITUDE))

summary(df_error_check$long_diff)
summary(df_error_check$lat_diff)

### Distance analysis visualization for Latitude & Longitude - Error distance analysis

df_error_check$PREDICTION = "empty"

for (i in 1:nrow(df_error_check)) {
  if (df_error_check[i,"long_diff"] <= 9 & df_error_check[i,"lat_diff"] <= 9) {
    df_error_check[i,"PREDICTION"] = "Great! :)"
  } else if (df_error_check[i,"long_diff"] <= 20 & df_error_check[i,"lat_diff"] <= 20) {
    df_error_check[i,"PREDICTION"] = "Ok... :|"
  } else if (df_error_check[i,"long_diff"] <= 50 & df_error_check[i,"lat_diff"] <= 50) {
    df_error_check[i,"PREDICTION"] = "Awfull :("
  } else {
    df_error_check[i,"PREDICTION"] = "Weird :S"
  }
}

df_error_check$PREDICTION = as.factor(x = df_error_check$PREDICTION)
summary(df_error_check$PREDICTION)

plot_ly(df_error_check) %>%
  add_markers(x = ~pred_LONGITUDE, y = ~pred_LATITUDE, z = ~pred_FLOOR, color = ~PREDICTION, colors = c("chartreuse", "firebrick1", "yellow1"), marker = list(size = 3)) %>%
  layout(title = "Distance analysis")

### Floor prediction analysis

df_error_check$WHERE_SHOULD_BE = "None"
for (i in 1:nrow(df_error_check)) {
  if (df_error_check[i,"real_FLOOR"] != df_error_check[i,"pred_FLOOR"]) {
    df_error_check[i,"WHERE_SHOULD_BE"] = paste("Should be floor", df_error_check[i, "real_FLOOR"])
  } else if (df_error_check[i,"real_FLOOR"] == df_error_check[i,"pred_FLOOR"]) {
    df_error_check[i,"WHERE_SHOULD_BE"] = "Good Prediction"
  }
}

df_error_check$WHERE_SHOULD_BE = factor(df_error_check$WHERE_SHOULD_BE)
summary(df_error_check$WHERE_SHOULD_BE)
plot_ly(df_error_check) %>%
  add_markers(x = ~pred_LONGITUDE, y = ~pred_LATITUDE, z = ~pred_FLOOR, color = ~WHERE_SHOULD_BE, marker = list(size = 3)) %>%
  layout(title = "In which floor should each capture be in?")

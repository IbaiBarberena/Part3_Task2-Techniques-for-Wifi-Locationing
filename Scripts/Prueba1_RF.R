pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN, class)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel <- readRDS("Data/trainmodel.rds")
# validationData <- read.csv2("Testing/validationData.csv")
validationData <- readRDS("Data/validationData.rds")

## Finding the best mtry for each model:
WAPS <- grep("WAP", names(trainmodel), value = T)
WAPS_BUILD <- grep("WAP|BUILDING", names(trainmodel), value = T)
WAPS_BUILD_LAT <- grep("WAP|BUILDING|LATITUDE", names(trainmodel), value = T)
WAPS_BUILD_LAT_LONG <- grep("WAP|BUILDING|LATITUDE|LONGITUDE", names(trainmodel), value = T)
WAPS_BUILD_LAT_LONG_FLOOR <- grep("WAP|BUILDING|LATITUDE|LONGITUDE|FLOOR", names(trainmodel), value = T)

##### CREATING THE PREDICTIVE MODELS FOR EACH COLUMN #### RF

# BUILDING #
rf2_building <- ranger(BUILDINGID~.,
                       data = trainmodel[,WAPS_BUILD],
                       mtry = 11)

# LATITUDE #
rf2_latitude <- ranger(LATITUDE~.,
                       data = trainmodel[,WAPS_BUILD_LAT],
                       mtry = 174)


# LONGITUDE #
rf2_longitude <- ranger(LONGITUDE~.,
                        data = trainmodel[,WAPS_BUILD_LAT_LONG],
                        mtry = 174)

# FLOOR #
rf2_floor <- ranger(FLOOR~.,
                    data = trainmodel[,WAPS_BUILD_LAT_LONG_FLOOR],
                    mtry = 44)


#### PREDICTIONS #####


# BUILDING #

validationData_1 <- validationData[,WAPS]

pred1_building <- predict(rf2_building, data = validationData_1)

Per_rf1_building <- confusionMatrix(pred1_building[["predictions"]],validationData$BUILDINGID)
Per_rf1_building

# LATITUDE #
validationData_1$BUILDINGID <- pred1_building[["predictions"]]

pred1_latitude <- predict(rf2_latitude, data = validationData_1)

Per_rf1_latitude <- postResample(pred1_latitude[["predictions"]],validationData$LATITUDE)
Per_rf1_latitude

# LONGITUDE #
validationData_1$LATITUDE <- pred1_latitude[["predictions"]]

pred1_longitude <- predict(rf2_longitude, data = validationData_1)

Per_rf1_longitude <- postResample(pred1_longitude[["predictions"]],validationData$LONGITUDE)
Per_rf1_longitude

# FLOOR #
validationData_1$LONGITUDE <- pred1_longitude[["predictions"]]

pred1_floor <- predict(rf2_floor, data = validationData_1)

validationData_1$FLOOR <- pred1_floor[["predictions"]]

Per_rf1_floor <- confusionMatrix(pred1_floor[["predictions"]],validationData$FLOOR)
Per_rf1_floor

# # FINAL PREDICTIONS #
# 
# Ibai_RF_casc <- validationData_1[,c(521:524)]
# orden <- c("LATITUDE", "LONGITUDE", "FLOOR", "BUILDINGID")
# Ibai_RF_casc <- Ibai_RF_casc[,orden]
# rm(orden)
# 
# ### If Building 0,1 and Floor 4, replacing them for Floor 3 ###
# 
# rows0 <- c()
# for(i in 1:nrow(Ibai_RF_casc)){
#   yes0 <- which(Ibai_RF_casc[i, "BUILDINGID"] == "0" & Ibai_RF_casc[i, "FLOOR"] == "4")
#   if (length(yes0) !=0) {
#     rows0 <- c(rows0,i)
#   }
# }
# Ibai_RF_casc[rows0,"FLOOR"] <- "3"
# 
# 
# rows1 <- c()
# for(i in 1:nrow(Ibai_RF_casc)){
#   yes1 <- which(Ibai_RF_casc[i, "BUILDINGID"] == "1" & Ibai_RF_casc[i, "FLOOR"] == "4")
#   if (length(yes1) !=0) {
#     rows1 <- c(rows1,i)
#   }
# }
# Ibai_RF_casc[rows1,"FLOOR"] <- "3"
# 
# 
# Ibai_RF_casc$BUILDINGID <- NULL
# 
# # write.csv2(Ibai_RF_casc, file = "Testing/Ibai_RF_casc.csv", row.names = FALSE)
# 


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

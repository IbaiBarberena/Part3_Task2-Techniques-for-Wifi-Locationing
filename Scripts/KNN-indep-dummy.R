## Without signals between -34 and 0 and -10 and -20
## With user6
## Modelling: no cascade
## RF

pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o, plyr, FNN, class)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainmodel <- readRDS("Data/trainmodel.rds")
trainphase <- readRDS("Data/trainphase.rds")
testphase <- readRDS("Data/testphase.rds")
validationData <- readRDS("Data/validationData.rds")

# densitymodel <- melt(trainmodel[, grep("WAP", names(trainmodel))])
# ggplot(densitymodel, aes(x=densitymodel$value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(0.000000001, 200)
# range(densitymodel$value)

##### MODELLING EACH CATEGORY #####

#### DUMMIFYING THE DATASETS ###

dmy1 <- dummyVars("~ .", data = trainmodel)
dmy2 <- dummyVars("~ .", data = trainphase)
dmy3 <- dummyVars("~ .", data = testphase)
dmy4 <- dummyVars("~ .", data = validationData)
d_trainmodel <- data.frame(predict(dmy1, newdata = trainmodel))
d_trainphase <- data.frame(predict(dmy2, newdata = trainphase))
d_testphase <- data.frame(predict(dmy3, newdata = testphase))
d_validationData <- data.frame(predict(dmy4, newdata = validationData))

## Finding the best mtry for each model:
WAPS <- colnames(d_trainmodel[,grep("WAP", names(trainmodel))])
WAPS_BUILD <- colnames(d_trainmodel[,grep("WAP|BUILDING", names(d_trainmodel))])
WAPS_BUILD_FLOOR <- colnames(d_trainmodel[,grep("WAP|BUILDING|FLOOR", names(d_trainmodel))])
WAPS_BUILD_FLOOR_LAT <- colnames(d_trainmodel[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(d_trainmodel))])
WAPS_BUILD_FLOOR_LAT_LONG <- colnames(d_trainmodel[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(d_trainmodel))])

WAPS2 <- colnames(d_testphase[,grep("WAP", names(d_testphase))])
WAPS_BUILD2 <- colnames(d_testphase[,grep("WAP|BUILDING", names(d_testphase))])
WAPS_BUILD_FLOOR2 <- colnames(d_testphase[,grep("WAP|BUILDING|FLOOR", names(d_testphase))])
WAPS_BUILD_FLOOR_LAT2 <- colnames(d_testphase[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(d_testphase))])
WAPS_BUILD_FLOOR_LAT_LONG2 <- colnames(d_testphase[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(d_testphase))])

WAPS3 <- colnames(d_validationData[,grep("WAP", names(d_validationData))])
WAPS_BUILD3 <- colnames(d_validationData[,grep("WAP|BUILDING", names(d_validationData))])
WAPS_BUILD_FLOOR3 <- colnames(d_validationData[,grep("WAP|BUILDING|FLOOR", names(d_validationData))])
WAPS_BUILD_FLOOR_LAT3 <- colnames(d_validationData[,grep("WAP|BUILDING|FLOOR|LATITUDE", names(d_validationData))])
WAPS_BUILD_FLOOR_LAT_LONG3 <- colnames(d_validationData[,grep("WAP|BUILDING|FLOOR|LATITUDE|LONGITUDE", names(d_validationData))])



## PREDICTIONS ##

## PHASE 1 ## Measuring the performance in trainphase

# BUILDING #

class_knn_building1 <- FNN::knn(train = d_trainmodel[c(1:520)], 
                                   cl= trainmodel$BUILDINGID,
                                   test= trainphase[,WAPS])
Per_knn_building1 <- confusionMatrix(class_knn_building1, trainphase$BUILDINGID)
Per_knn_building1

# FLOOR #

class_knn_floor1 <- FNN::knn(train = d_trainmodel[c(1:523)], 
                                cl= trainmodel$FLOOR,
                                test= d_trainphase[,WAPS_BUILD])

Per_knn_floor1 <- confusionMatrix(class_knn_floor1, trainphase$FLOOR)
Per_knn_floor1


# LATITUDE #

class_knn_latitude1 <- FNN::knn.reg(train = d_trainmodel[c(1:528)], 
                                   y= trainmodel$LATITUDE,
                                   test= d_trainphase[,WAPS_BUILD_FLOOR])
Per_knn_latitude1 <- postResample(class_knn_latitude1[["pred"]], trainphase$LATITUDE)
Per_knn_latitude1

# LONGITUDE #

class_knn_latitude1 <- FNN::knn.reg(train = d_trainmodel[c(1:529)], 
                                   y= trainmodel$LONGITUDE,
                                   test= d_trainphase[,WAPS_BUILD_FLOOR_LAT])
Per_knn_longitude1 <- postResample(class_knn_latitude1[["pred"]],trainphase$LONGITUDE)
Per_knn_longitude1

## PHASE 2 ## Measuring the performance in testphase

class_knn_building2 <- FNN::knn(train = d_trainmodel[c(1:520)], 
                               cl= trainmodel$BUILDINGID,
                               test= testphase[,WAPS2])
Per_knn_building2 <- confusionMatrix(class_knn_building2, testphase$BUILDINGID)
Per_knn_building2

# FLOOR #

class_knn_floor2 <- FNN::knn(train = d_trainmodel[c(1:523)], 
                            cl= trainmodel$FLOOR,
                            test= d_testphase[,WAPS_BUILD2])

Per_knn_floor2 <- confusionMatrix(class_knn_floor2, testphase$FLOOR)
Per_knn_floor2


# LATITUDE #

class_knn_latitude2 <- FNN::knn.reg(train = d_trainmodel[c(1:528)], 
                                   y= trainmodel$LATITUDE,
                                   test= d_testphase[,WAPS_BUILD_FLOOR2])
Per_knn_latitude2 <- postResample(class_knn_latitude2[["pred"]], testphase$LATITUDE)
Per_knn_latitude2

# LONGITUDE #

class_knn_longitude2 <- FNN::knn.reg(train = d_trainmodel[c(1:529)], 
                                   y= trainmodel$LONGITUDE,
                                   test= d_testphase[,WAPS_BUILD_FLOOR_LAT2])
Per_knn_longitude2 <- postResample(class_knn_longitude2[["pred"]],testphase$LONGITUDE)
Per_knn_longitude2


## PHASE 3 ## Measuring the performance in validationData

# BUILDING #

class_knn_building3 <- FNN::knn(train = d_trainmodel[c(1:520)], 
                                cl= trainmodel$BUILDINGID,
                                test= d_validationData[,WAPS3])
Per_knn_building3 <- confusionMatrix(class_knn_building3, validationData$BUILDINGID)
Per_knn_building3

# FLOOR #

class_knn_floor3 <- FNN::knn(train = d_trainmodel[c(1:523)], 
                             cl= trainmodel$FLOOR,
                             test= d_validationData[,WAPS_BUILD3])

Per_knn_floor3 <- confusionMatrix(class_knn_floor3, validationData$FLOOR)
Per_knn_floor3


# LATITUDE #

class_knn_latitude3 <- FNN::knn.reg(train = d_trainmodel[c(1:528)], 
                                    y= trainmodel$LATITUDE,
                                    test= d_validationData[,WAPS_BUILD_FLOOR3])
Per_knn_latitude3 <- postResample(class_knn_latitude3[["pred"]], validationData$LATITUDE)
Per_knn_latitude3

# LONGITUDE #

class_knn_longitude3 <- FNN::knn.reg(train = d_trainmodel[c(1:529)], 
                                     y= trainmodel$LONGITUDE,
                                     test= d_validationData[,WAPS_BUILD_FLOOR_LAT3])
Per_knn_longitude3 <- postResample(class_knn_longitude3[["pred"]],validationData$LONGITUDE)
Per_knn_longitude3

# Gathering the data #

df_knn_error <- data.frame(BUILDINGID = class_knn_building3, FLOOR = class_knn_floor3, LATITUDE = class_knn_latitude3[["pred"]], LONGITUDE = class_knn_longitude3[["pred"]])
names(df_knn_error) <- paste("pred", names(df_knn_error), sep = "_")
df_knn_error <- cbind(validationData[,c(521:524)], df_knn_error)
names(df_knn_error)[1:4] <- paste("real", names(df_knn_error)[1:4], sep = "_")

df_knn_error$long_diff = abs(abs(df_knn_error$real_LONGITUDE) - abs(df_knn_error$pred_LONGITUDE))
df_knn_error$lat_diff = abs(abs(df_knn_error$real_LATITUDE) - abs(df_knn_error$pred_LATITUDE))

summary(df_knn_error$long_diff)
summary(df_knn_error$lat_diff)


### Distance analysis visualization for Latitude & Longitude - Error distance analysis

df_knn_error$PREDICTION = "empty"

for (i in 1:nrow(df_knn_error)) {
  if (df_knn_error[i,"long_diff"] <= 9 & df_knn_error[i,"lat_diff"] <= 9) {
    df_knn_error[i,"PREDICTION"] = "Great! :)"
  } else if (df_knn_error[i,"long_diff"] <= 20 & df_knn_error[i,"lat_diff"] <= 20) {
    df_knn_error[i,"PREDICTION"] = "Ok... :|"
  } else if (df_knn_error[i,"long_diff"] <= 50 & df_knn_error[i,"lat_diff"] <= 50) {
    df_knn_error[i,"PREDICTION"] = "Awfull :("
  } else {
    df_knn_error[i,"PREDICTION"] = "Weird :S"
  }
}

df_knn_error$PREDICTION = as.factor(x = df_knn_error$PREDICTION)
summary(df_knn_error$PREDICTION)

plot_ly(df_knn_error) %>%
  add_markers(x = ~pred_LONGITUDE, y = ~pred_LATITUDE, z = ~pred_FLOOR, color = ~PREDICTION, colors = c("chartreuse", "firebrick1", "yellow1"), marker = list(size = 3)) %>%
  layout(title = "Distance analysis")

### Floor prediction analysis

df_knn_error$WHERE_SHOULD_BE = "None"
for (i in 1:nrow(df_knn_error)) {
  if (df_knn_error[i,"real_FLOOR"] != df_knn_error[i,"pred_FLOOR"]) {
    df_knn_error[i,"WHERE_SHOULD_BE"] = paste("Should be floor", df_knn_error[i, "real_FLOOR"])
  } else if (df_knn_error[i,"real_FLOOR"] == df_knn_error[i,"pred_FLOOR"]) {
    df_knn_error[i,"WHERE_SHOULD_BE"] = "Good Prediction"
  }
}

df_knn_error$WHERE_SHOULD_BE = factor(df_knn_error$WHERE_SHOULD_BE)
summary(df_knn_error$WHERE_SHOULD_BE)
plot_ly(df_knn_error) %>%
  add_markers(x = ~pred_LONGITUDE, y = ~pred_LATITUDE, z = ~pred_FLOOR, color = ~WHERE_SHOULD_BE, marker = list(size = 3)) %>%
  layout(title = "In which floor should each capture be in?")


### Plotear distribucion de los errores!

ggplot(df_knn_error, aes(x=df_knn_error$long_diff))+
  geom_density(color="darkblue", fill="lightblue") + xlim(20, 200)

ggplot(df_knn_error, aes(x=df_knn_error$lat_diff))+
  geom_density(color="darkblue", fill="lightblue") + xlim(20, 200)

over50_error <- filter(df_knn_error, df_knn_error$long_diff > 50)

## Intentar averiguar de que usuario/WAP provienen la mayoria de los errores e intenta eliminar el usuario/WAP.

## which(trainingData[,c(1:520)] < -100). Filtrar y ver de que edificio/building/ user ID provienen

## https://www.metageek.com/training/resources/wifi-signal-strength-basics.html Check logarithm
## Preguntar a Ignacio sobre logaritmo en dBs

aa <- c()

for(i in 1:length(grep("WAP", names(trainingData)))){
  
  s <- which(trainingData[,i] < -100)
  if (length(s) !=0) {
    aa <- c(aa,s)
  }
}

aa <- unique(aa)

pr <- trainingData[aa,]

plot_ly(x = pr$USERID, type = "histogram")
plot_ly(x = pr$, type = "histogram")
pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d, ranger, data.table, h2o)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainingData <- readRDS("Data/trainingData.rds")
validationData <- readRDS("Data/validationData.rds")
totalData <- readRDS("Data/totalData.rds")


notused <- c("SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")
trainingData[,notused] <- NULL
validationData[,notused] <- NULL
totalData[,notused] <- NULL

##### Sampling the trainingData #### Phase 1 and 2

set.seed(123)

sample <- createDataPartition(trainingData$FLOOR, p= 0.5, list= FALSE)

train_sample <- trainingData[sample,]

length(unique(paste(train_sample$LONGITUDE, train_sample$LATITUDE, train_sample$FLOOR)))

# plot_ly(train_sample, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,
#   colors = c("#E69F00"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = "Longitude"),
#                       yaxis = list(title = "Latitude"),
#                       zaxis = list(title = "Floor")))

##This sampling represents 923 unique points out of 933 possible points (99%)

WAPS <- colnames(train_sample[,grep("WAP", names(train_sample))])

order <- c(WAPS, "BUILDINGID", "FLOOR", "LONGITUDE", "LATITUDE")

train_sample <- train_sample[,order]

rm(order)

## Splitting into Training and Testing

partition <- createDataPartition(train_sample$FLOOR, p=0.75, list = FALSE)

trainphase1 <- train_sample[partition,]
testphase2 <- train_sample[-partition,]

# delete <- c("LONGITUDE", "LATITUDE", "BUILDINGID", "TIMESTAMP", "location")
# 
# trainphase1_floor <- trainphase1
# trainphase1_floor[, delete] <- NULL

# rm(delete)

# Setting the parameters for creating the  predictive models:


#################### FLOOR PREDICTION #######################

####### 1. Random Forest #############
#Floor
mtry <- tuneRF(x= trainphase1[1:520], y= trainphase1$FLOOR, ntree= 100, plot = F) # mtry = 88

WAP_FLOOR <- grep("WAP|FLOOR", names(train_sample))

rf_floor <- ranger(FLOOR~., 
                data = trainphase1[,WAP_FLOOR],
                mtry = 88)

pred.floor <- predict(rf_floor, data = trainphase1)

CM_rf_floor <- confusionMatrix(pred.floor[["predictions"]],trainphase1$FLOOR)
CM_rf_floor

#Longitude

trainphase1_floor <- trainphase1[,WAPS]
trainphase1_floor$FLOOR <- pred.floor[["predictions"]]
trainphase1_floor$LONGITUDE <- trainphase1$LONGITUDE

# mtry2 <- tuneRF(x= trainphase1_floor[1:521], y= trainphase1_floor$LONGITUDE, ntree= 100) # mtry = 87

WAP_FLOOR_LONGITUDE <- grep("WAP|FLOOR|LONGITUDE", names(train_sample))

rf_LONGITUDE <- ranger(LONGITUDE~., 
                   data = trainphase1_floor, mtry= 87)
pred.LONGITUDE <- predict(rf_LONGITUDE, data = trainphase1_floor)

pred_trainLONGITUDE <- postResample(pred.LONGITUDE[["predictions"]], trainphase1$LONGITUDE)
pred_trainLONGITUDE

#Latitude

trainphase1_LONGITUDE <- trainphase1_floor
trainphase1_LONGITUDE$LONGITUDE <- pred.LONGITUDE[["predictions"]]
trainphase1_LONGITUDE$LATITUDE <- trainphase1$LATITUDE

# mtry3 <- tuneRF(trainphase1_LONGITUDE, trainphase1_LONGITUDE$LATITUDE, ntree= 100)  # mtry = 348

rf_LATITUDE <- ranger(LATITUDE~., 
                       data = trainphase1_LONGITUDE, mtry = 348)

pred.LATITUDE <- predict(rf_LATITUDE, data = trainphase1_LONGITUDE)

pred_train.LATITUDE <- postResample(pred.LATITUDE[["predictions"]], trainphase1$LATITUDE)
pred_train.LATITUDE

pred_trainphase1 <- trainphase1_LONGITUDE

pred_trainphase1$LATITUDE <- pred.LATITUDE[["predictions"]]




#### Phase 2: Testing with the Testing set ####

## Floor ##

pred.test.FLOOR <- predict(rf_floor, data = testphase2)

CM_rf_floor_test <- confusionMatrix(pred.test.FLOOR[["predictions"]],testphase2$FLOOR)
CM_rf_floor_test

testphase2_FLOOR <- testphase2[,WAPS]
testphase2_FLOOR$pred_FLOOR <- pred.test.FLOOR[["predictions"]]

# LONGITUDE ##

pred.test.LONGITUDE <- predict(rf_LONGITUDE, data = testphase2_FLOOR)
post.test.LONGITUDE <- postResample(pred.test.LONGITUDE[["predictions"]], testphase2$LONGITUDE)
post.test.LONGITUDE

testphase2_LONGITUDE <- testphase2_FLOOR
testphase2_LONGITUDE$pred_LONGITUDE <- pred.test.LONGITUDE[["predictions"]]


# LATITUDE ##

pred.test.LATITUDE <- predict(rf_LATITUDE, data = testphase2_LONGITUDE)
post.test.LATITUDE <- postResample(pred.test.LATITUDE[["predictions"]], testphase2$LATITUDE)
post.test.LATITUDE

pred_testphase2 <- testphase2_LONGITUDE
pred_testphase2$pred_LATITUDE <- pred.test.LATITUDE[["predictions"]]

### Gathering the testing data and predictions in order to plot the errors:

setnames(pred_testphase2, old=c("pred_FLOOR","pred_LONGITUDE", "pred_LATITUDE"), new=c("FLOOR", "LONGITUDE", "LATITUDE"))

totaltesting <- totaldata <- gdata::combine(testphase2[,c(1:523)], pred_testphase2)

plot_ly(totaldata, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color= ~source,
  colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


##### Testing the models with unseen data: Validation data ####

a <- predict(rf_LONGITUDE, validationData)

confusionMatrix(a[["predictions"]], validationData$FLOOR)

#### Getting to know H20 ####

# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/master/4762/R")

# Finally, let's load H2O and start up an H2O cluster
# library(h2o)
h2o.init()



h2o.trainphase1 <- as.h2o(trainphase1)
h2o.testphase2 <- as.h2o(testphase2)

kmeans = h2o.kmeans(h2o.trainphase1, estimate_k = TRUE, x= WAPS)
print(kmeans)

h2o.kmeans.pred <- h2o.predict(kmeans, h2o.testphase2)

h2o.confusionMatrix(h2o.kmeans.pred
                    , testphase2)

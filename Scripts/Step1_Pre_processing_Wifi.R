pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainingData <- as_tibble(read.csv2("Data/trainingData.csv", sep= ",", stringsAsFactors=FALSE))
validationData <- as_tibble(read.csv2("Data/validationData.csv", sep= ",", stringsAsFactors=FALSE))

## Pre-processing the data ##

# anyNA(trainingData)
# anyNA(validationData)


ffeatures <-c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID")

trainingData[,ffeatures] <- apply(trainingData[,ffeatures], 2, as.factor)
validationData[,ffeatures] <- apply(validationData[,ffeatures], 2, as.factor)

rm(ffeatures)

# Changing the TimeStamp from UNIX units to Date-Time units
trainingData$TIMESTAMP <- anytime(trainingData$TIMESTAMP)
validationData$TIMESTAMP <- anytime(validationData$TIMESTAMP)



zerovarianze <- apply(trainingData[,c(1:520)], 1, mean)

zerovarianze <- which(zerovarianze == 100)

trainingData <- trainingData[-zerovarianze,]

# ## Tracking different locations ##
# 
# locations <- trainingData %>% 
#   distinct(BUILDINGID, FLOOR, SPACEID, RELATIVEPOSITION)

join <- c("BUILDINGID","FLOOR")

trainingData$location <- apply(trainingData[, join], 1, paste, collapse= "-")
validationData$location <- apply(validationData[, join], 1, paste, collapse= "-")

rm(join)

## Removing duplicates ##

trainingData <- trainingData[!duplicated(trainingData), ]

# Removing User 6 and 14

# trainingData <- filter(trainingData, trainingData$USERID != 6)
trainingData <- filter(trainingData, trainingData$USERID != 14)

# Plotting the density of the WAP
# Train dataset

# boxplot
# boxplot <- filter(density, density$value != 100)
#boxplot(boxplot$value)
# rm(boxplot)

## Validation dataset
# WAPdens2 <- validationData[, grep("WAP", names(validationData))]
# 
# density2 <- melt(WAPdens2)

# dev.off()
# ggplot(density2, aes(x=density2$value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


# boxplot
# boxplot2 <- filter(density2, density2$value != 100)
# boxplot(boxplot2$value)

# range(boxplot2$value)

# rm(boxplot2)

### We have dBm signals that are higher than -34, which is very unusual.
# Analyzing which WAP connections have these values:
# densityfilt <- filter(density, density$value > -34 & density$value < 0)
# 
# plot_ly(x = densityfilt$variable, type = "histogram")

#Most of the values btween -34 and 0 dBm come from WAP087, WAP065, WAP066
# ggplot(trainingData, aes(x=WAP087))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# 
# ggplot(trainingData, aes(x=WAP065))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# 
# ggplot(trainingData, aes(x=WAP066))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

# WAPnames <- colnames(trainingData[,grep("WAP", names(trainingData))])

rows <- c()
column <- c()
for(i in 1:length(grep("WAP", names(trainingData)))){
  
  s <- which(trainingData[,i] > -30 & trainingData[,i] < 0)
  if (length(s) !=0) {
    rows <- c(rows,s)
    column <- c(column, i)
  }
}

rows <- unique(rows)

train_maxsignal <- trainingData[rows,]



trainingData <- trainingData[-rows,]
range(trainingData[, grep("WAP", names(trainingData))])

# plot_ly(x = train_maxsignal$PHONEID, type = "histogram")
# plot_ly(x = train_maxsignal$location, type = "histogram")
# plot_ly(x = train_maxsignal$TIMESTAMP, type = "histogram")
# plot_ly(x = train_maxsignal$USERID, type = "histogram")
# plot_ly(x = train_maxsignal$RELATIVEPOSITION, type = "histogram")
# plot_ly(x = train_maxsignal$SPACEID, type = "histogram")

#Location of trainingset and Validationset
# plot_ly(x = trainingData$location, type = "histogram")
# plot_ly(x = validationData$location, type = "histogram")


# We see that 199 WAP signals out of 727 are higher than -34. Let's filter the data:

# 
# trainingData_2_4 <- filter(trainingData, trainingData$location == "2-4")
# 
# 
# trainingData_2_4_us6 <- filter(trainingData, trainingData$location == "2-4" & trainingData$USERID == "6")
# 
# WAPus6 <- trainingData_2_4_us6[, grep("WAP", names(validationData))]
# trainingData_2_4_us6 <- melt(WAPus6)
# trainingData_2_4_us6 <- filter(trainingData_2_4_us6, trainingData_2_4_us6$value != 100)

# ggplot(trainingData_2_4_us6, aes(x=value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

#### Takeaway: Even though USER 6 has many WIFI signs between -34 and 0, it is still an important USER.



## We see that the User 6 has uncommon dBs signals. Let´s delve into all the WAP signals of this user:

# user14 <- filter(trainingData, trainingData$USERID == 14)
# 
# 
# densUser14 <- melt(user14[, grep("WAP|location", names(user14))])
# 
# # plot_ly(x = densUser14$location, y= densUser14$value,  type = "bar")
# 
# ggplot(densUser14, aes(x=value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# 
# 
# user14wap_loc <- melt(user14, grep("WAP|location", names(user14)))
# user14wap_loc_r <- filter(user14wap_loc, user14wap_loc$value < 0 & user14wap_loc$value > -34)
# plot_ly(x = user14wap_loc_r$location, y= user14wap_loc_r$value,  type = "bar")


# user6wap087_melt <- filter(densUser6, densUser6$variable == "WAP087")

# ggplot(user6wap087_melt, aes(x=user6wap087_melt$value)) +
  # geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


## CONCLUSIONS USER 6

# All the Wifi signals of this user comes locations 2-3 and 2-4
# User 6 has in total 566 RSSI signs between -34 an 0. 13,25% of them come from WAP087. This values range from -10 and -20




##### DEALING WITH ALL THE NO SIGNAL VALUES ########


trainingData <- replace(trainingData, trainingData == 100, 0)
validationData <- replace(validationData, validationData == 100, 0)

trainingData <- replace(trainingData, trainingData == -104, 0.001)
validationData <- replace(validationData, validationData == -104, 0.001)


# All the WAP signal have been moved to positive
for(i in 1:length(grep("WAP", names(trainingData)))){
  l <- which(trainingData[,i] != 0)
  trainingData[l,i] <- trainingData[l,i] + 104
}

for(i in 1:length(grep("WAP", names(validationData)))){
  l <- which(validationData[,i] != 0)
  validationData[l,i] <- validationData[l,i] + 104
}

trainingData <- replace(trainingData, trainingData == 104.001, 0.001)
validationData <- replace(validationData, validationData == 104.001, 0.001)

range(trainingData[,c(1:520)])
range(validationData[,c(1:520)])

# densval <- melt(validationData[, grep("WAP", names(validationData))])
# densval2 <- melt(trainingData[, grep("WAP", names(validationData))])
# 
# ggplot(densval, aes(x=densval2$value)) +
#   geom_density(color="darkblue", fill="lightblue") + xlim(0.00001,5)
# 
#Normalizing the data
# trainingData[,c(1:520)] <- trainingData[,c(1:520)]/104
# validationData[,c(1:520)] <- validationData[,c(1:520)]/104




### TOTAL DATA ###

# zerovarianze <- which(apply(trainingData[,c(1:520)], 2, var) == 0)
# 
# trainingData <- trainingData[,-zerovarianze]
# validationData <- validationData[,-zerovarianze]
# 
# totaldata <- gdata::combine(trainingData, validationData)
# 
# zerowaps <- which(apply(totaldata[,c(1:520)], 2, var) == 0) # Combining the Train and Validation datasets there are NOT columns with 0 variance



#### GATHERING ALL THE DATASET IN ORDER TO GLANCE THE DISTRIBUTION OF THE DATA BETWEEN THE BUILDINGS AND FLOORS ###


# plot_ly(totaldata, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                        yaxis = list(title = "Latitude"),
#                       zaxis = list(title = "Floor")))



### Changing variable types and names:

trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)

# levels(trainingData$FLOOR)[1] <- 'Floor_0'
# levels(trainingData$FLOOR)[2] <- 'Floor_1'
# levels(trainingData$FLOOR)[3] <- 'Floor_2'
# levels(trainingData$FLOOR)[4] <- 'Floor_3'
# levels(trainingData$FLOOR)[5] <- 'Floor_4'
# 
# levels(trainingData$BUILDINGID)[1] <- 'Building_0'
# levels(trainingData$BUILDINGID)[2] <- 'Building_1'
# levels(trainingData$BUILDINGID)[3] <- 'Building_2'

levels(trainingData$FLOOR)[1] <- '0'
levels(trainingData$FLOOR)[2] <- '1'
levels(trainingData$FLOOR)[3] <- '2'
levels(trainingData$FLOOR)[4] <- '3'
levels(trainingData$FLOOR)[5] <- '4'

levels(trainingData$BUILDINGID)[1] <- '0'
levels(trainingData$BUILDINGID)[2] <- '1'
levels(trainingData$BUILDINGID)[3] <- '2'

validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)

# levels(validationData$FLOOR)[1] <- 'Floor_0'
# levels(validationData$FLOOR)[2] <- 'Floor_1'
# levels(validationData$FLOOR)[3] <- 'Floor_2'
# levels(validationData$FLOOR)[4] <- 'Floor_3'
# levels(validationData$FLOOR)[5] <- 'Floor_4'
# 
# levels(validationData$BUILDINGID)[1] <- 'Building_0'
# levels(validationData$BUILDINGID)[2] <- 'Building_1'
# levels(validationData$BUILDINGID)[3] <- 'Building_2'

levels(validationData$FLOOR)[1] <- '0'
levels(validationData$FLOOR)[2] <- '1'
levels(validationData$FLOOR)[3] <- '2'
levels(validationData$FLOOR)[4] <- '3'
levels(validationData$FLOOR)[5] <- '4'

levels(validationData$BUILDINGID)[1] <- '0'
levels(validationData$BUILDINGID)[2] <- '1'
levels(validationData$BUILDINGID)[3] <- '2'

trainingData$LATITUDE <- as.numeric(trainingData$LATITUDE)
trainingData$LONGITUDE <- as.numeric(trainingData$LONGITUDE)

validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)


### BUILD 2 FLOOR 4: There are 8 conection sets from ValidationData that can be use in the trainset, as these locations are now shown in the TrainingData

valid2_4 <- filter(validationData, validationData$location == "2-4")
train2_4 <- filter(trainingData, trainingData$location == "2-4")
valid2_4$numbers <- c(1:39)
train2_4$numbers <- c(1:529)
t2_4 <- gdata::combine(train2_4, valid2_4)

# plot_ly(t2_4, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers, 'User:', USERID),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                        yaxis = list(title = "Latitude"),
#                       zaxis = list(title = "Floor")))

addtrain <- valid2_4[c(20,17,39,21,25,26,6,31,19),]

addtrain$numbers <- NULL

rm(valid2_4,train2_4, t2_4)

trainingData <- rbind(trainingData, addtrain)







# validationData <- anti_join(validationData, addtrain)
rm(addtrain)

### BUILD 1 FLOOR 1

valid1_1 <- filter(validationData, validationData$location == "1-1")
train1_1 <- filter(trainingData, trainingData$location == "1-1")
valid1_1$numbers <- c(1:143)
train1_1$numbers <- c(1:957)#957
t1_1 <- gdata::combine(train1_1, valid1_1)

# plot_ly(t1_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

añadir <- c(81,85,83,87,24,74,76,71,27,126,62,123,59,127,55,54,57,96,52,68,37,51,50,109,1,49,48,103,31,30,29,110,108,104,113,4,140,40,142,43)
addtrain2 <- valid1_1[añadir,]

addtrain2$numbers <- NULL

rm(valid1_1,train1_1, t1_1)

trainingData <- rbind(trainingData, addtrain2)
rm(addtrain2)


### BUILD 1 FLOOR 2

valid1_2 <- filter(validationData, validationData$location == "1-2")
train1_2 <- filter(trainingData, trainingData$location == "1-2")
valid1_2$numbers <- c(1:87)
train1_2$numbers <- c(1:1396)
t1_2 <- gdata::combine(train1_2, valid1_2)

# plot_ly(t1_2, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain3 <- valid1_2[c(62,53,60,64,49,16,85,6,8,47,23,43,35,44,63,65,77,51,37,24,30,75,2,3,4),]

addtrain3$numbers <- NULL

rm(valid1_2,train1_2, t1_2)

trainingData <- rbind(trainingData, addtrain3)

rm(addtrain3)

### BUILD 1 FLOOR 0

valid1_0 <- filter(validationData, validationData$location == "1-0")
train1_0 <- filter(trainingData, trainingData$location == "1-0")
valid1_0$numbers <- c(1:30)
train1_0$numbers <- c(1:1194)
t1_0 <- gdata::combine(train1_0, valid1_0)

# plot_ly(t1_0, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain4 <- valid1_0[c(28,22,5,12,1,2,10,23,25,11),]

addtrain4$numbers <- NULL

rm(valid1_0,train1_0, t1_0)

trainingData <- rbind(trainingData, addtrain4)

rm(addtrain4)

### BUILD 1 FLOOR 3

valid1_3 <- filter(validationData, validationData$location == "1-3")
train1_3 <- filter(trainingData, trainingData$location == "1-3")
valid1_3$numbers <- c(1:47)
train1_3$numbers <- c(1:909)
t1_3 <- gdata::combine(train1_3, valid1_3)

plot_ly(t1_3, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
        text= ~paste('number:', numbers),
        colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
  add_markers() %>%
  layout(scene= list(xaxis = list(title = "Longitude"),
                     yaxis = list(title = "Latitude"),
                     zaxis = list(title = "Floor")))

addtrain5 <- valid1_3[c(29, 30, 25, 27, 4, 44, 11, 41),]

addtrain5$numbers <- NULL

rm(valid1_3,train1_3, t1_3)

trainingData <- rbind(trainingData, addtrain5)

rm(addtrain5)

### BUILD 0 FLOOR 3

valid0_3 <- filter(validationData, validationData$location == "0-3")
train0_3 <- filter(trainingData, trainingData$location == "0-3")
valid0_3$numbers <- c(1:85)
train0_3$numbers <- c(1:1390)
t0_3 <- gdata::combine(train0_3, valid0_3)

# plot_ly(t0_3, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain6 <- valid0_3[c(38,55,72,75),]

addtrain6$numbers <- NULL

rm(valid0_3,train0_3, t0_3)

trainingData <- rbind(trainingData, addtrain6)

rm(addtrain6)

### BUILD 0 FLOOR 2

valid0_2 <- filter(validationData, validationData$location == "0-2")
train0_2 <- filter(trainingData, trainingData$location == "0-2")
valid0_2$numbers <- c(1:165)
train0_2$numbers <- c(1:1443)
t0_2 <- gdata::combine(train0_2, valid0_2)

# plot_ly(t0_2, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain7 <- valid0_2[c(23,22,60,4,79,27,66,165,121),]

addtrain7$numbers <- NULL

rm(valid0_2,train0_2, t0_2)

trainingData <- rbind(trainingData, addtrain7)

rm(addtrain7)

### BUILD 0 FLOOR 1

valid0_1 <- filter(validationData, validationData$location == "0-1")
train0_1 <- filter(trainingData, trainingData$location == "0-1")
valid0_1$numbers <- c(1:208)
train0_1$numbers <- c(1:1356)
t0_1 <- gdata::combine(train0_1, valid0_1)

# plot_ly(t0_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain8 <- valid0_1[c(2,97,150,73,70,128,179,199,153,168,164),]

addtrain8$numbers <- NULL

rm(valid0_1,train0_1, t0_1)

trainingData <- rbind(trainingData, addtrain8)

rm(addtrain8)

### BUILD 0 FLOOR 0

valid0_0 <- filter(validationData, validationData$location == "0-0")
train0_0 <- filter(trainingData, trainingData$location == "0-0")
valid0_0$numbers <- c(1:78)
train0_0$numbers <- c(1:1055)
t0_0 <- gdata::combine(train0_0, valid0_0)

# plot_ly(t0_0, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain9 <- valid0_0[c(55, 50, 51, 62, 68, 72, 43, 61, 74),]

addtrain9$numbers <- NULL

rm(valid0_0,train0_0, t0_0)

trainingData <- rbind(trainingData, addtrain9)

rm(addtrain9)

### BUILD 2 FLOOR 0

valid2_0 <- filter(validationData, validationData$location == "2-0")
train2_0 <- filter(trainingData, trainingData$location == "2-0")
valid2_0$numbers <- c(1:24)
train2_0$numbers <- c(1:1904)
t2_0 <- gdata::combine(train2_0, valid2_0)

# plot_ly(t2_0, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain10 <- valid2_0[c(8,11,12),]

addtrain10$numbers <- NULL

rm(valid2_0,train2_0, t2_0)

trainingData <- rbind(trainingData, addtrain10)

rm(addtrain10)

### BUILD 2 FLOOR 1

valid2_1 <- filter(validationData, validationData$location == "2-1")
train2_1 <- filter(trainingData, trainingData$location == "2-1")
valid2_1$numbers <- c(1:111)
train2_1$numbers <- c(1:2159)
t2_1<- gdata::combine(train2_1, valid2_1)

# plot_ly(t2_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain11 <- valid2_1[c(97,60,78,81,5,70,4),]

addtrain11$numbers <- NULL

rm(valid2_1,train2_1, t2_1)

trainingData <- rbind(trainingData, addtrain11)

rm(addtrain11)

### BUILD 2 FLOOR 2

valid2_2 <- filter(validationData, validationData$location == "2-2")
train2_2 <- filter(trainingData, trainingData$location == "2-2")
valid2_2$numbers <- c(1:54)
train2_2$numbers <- c(1:966)
t2_2<- gdata::combine(train2_2, valid2_2)

# plot_ly(t2_2, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain12 <- valid2_2[c(53,48,20,6,39,49),]

addtrain12$numbers <- NULL

rm(valid2_2,train2_2, t2_2)

trainingData <- rbind(trainingData, addtrain12)

rm(addtrain12)

### BUILD 2 FLOOR 3

valid2_3 <- filter(validationData, validationData$location == "2-3")
train2_3 <- filter(trainingData, trainingData$location == "2-3")
valid2_3$numbers <- c(1:40)
train2_3$numbers <- c(1:1992)
t2_3<- gdata::combine(train2_3, valid2_3)

# plot_ly(t2_3, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

addtrain13 <- valid2_3[c(),]

addtrain13$numbers <- NULL

rm(valid2_3,train2_3, t2_3)

trainingData <- rbind(trainingData, addtrain13)

rm(addtrain13)




### Checking PhoneDevice 13. It does not exist. Let's remove them

phone17 <- filter(trainingData, trainingData$PHONEID == 17)

plot_ly(x = phone17$location, type = "histogram")

trainingData <- filter(trainingData, trainingData$PHONEID != 17)

## Information to add: 1-0, 1-1, 2-4

### BUILD 1 FLOOR 1

valid1_1 <- filter(validationData, validationData$location == "1-1")
train1_1 <- filter(trainingData, trainingData$location == "1-1")
valid1_1$numbers <- c(1:143)
train1_1$numbers <- c(1:882)#957
t1_1 <- gdata::combine(train1_1, valid1_1)

# plot_ly(t1_1, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

añadirotra <- c(119, 66, 64, 53, 130, 122, 120, 8, 97, 129, 134, 9, 92, 10, 115, 131, 124, 118, 132, 125, 93, 3, 22, 38, 21, 5, 20, 12, 114, 65, 141, 101, 67, 79, 7, 44, 70)
añadirotra <- valid1_1[añadir,]

añadirotra$numbers <- NULL

rm(valid1_1,train1_1, t1_1)

trainingData <- rbind(trainingData, añadirotra)
rm(añadirotra)
anyNA(trainingData)

### BUILD 1 FLOOR 0

valid1_0 <- filter(validationData, validationData$location == "1-0")
train1_0 <- filter(trainingData, trainingData$location == "1-0")
valid1_0$numbers <- c(1:30)
train1_0$numbers <- c(1:1167)
t1_0 <- gdata::combine(train1_0, valid1_0)

# plot_ly(t1_0, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                      yaxis = list(title = "Latitude"),
#                      zaxis = list(title = "Floor")))

añadirotra2 <- c(3, 24)
añadirotra2 <- valid1_0[añadirotra2,]

añadirotra2$numbers <- NULL

rm(valid1_0,train1_0, t1_0)

trainingData <- rbind(trainingData, añadirotra2)
rm(añadirotra2)

anyNA(trainingData)
### BUILD 2 FLOOR 4:

valid2_4 <- filter(validationData, validationData$location == "2-4")
train2_4 <- filter(trainingData, trainingData$location == "2-4")
valid2_4$numbers <- c(1:39)
train2_4$numbers <- c(1:471)
t2_4 <- gdata::combine(train2_4, valid2_4)

# plot_ly(t2_4, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         text= ~paste('number:', numbers, 'User:', USERID),
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                        yaxis = list(title = "Latitude"),
#                       zaxis = list(title = "Floor")))

addtrain <- valid2_4[c(18, 38, 16, 36, 35, 14, 3, 7, 1, 34, 30, 29, 32, 5, 4, 24, 33),]

addtrain$numbers <- NULL

rm(valid2_4,train2_4, t2_4)

trainingData <- rbind(trainingData, addtrain)

rm(addtrain)


### Saving the datasets:

saveRDS(trainingData,"Data/trainingData.rds")
saveRDS(validationData,"Data/validationData.rds")

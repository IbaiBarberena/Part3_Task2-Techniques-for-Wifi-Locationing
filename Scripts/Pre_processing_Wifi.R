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

## Plotting the density of the WAP
## Train dataset
WAPdens <- trainingData[, grep("WAP", names(trainingData))]

density <- melt(WAPdens)

# ggplot(density, aes(x=density$value))+
  # geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# boxplot
boxplot <- filter(density, density$value != 100)
# boxplot(boxplot$value)
rm(boxplot)

## Validation dataset
WAPdens2 <- validationData[, grep("WAP", names(validationData))]

density2 <- melt(WAPdens2)

dev.off()
# ggplot(density2, aes(x=density2$value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


# boxplot
boxplot2 <- filter(density2, density2$value != 100)
# boxplot(boxplot2$value)

range(boxplot2$value)

rm(boxplot2)

### We have dBm signals that are higher than -34, which is very unusual.
# Analyzing which WAP connections have these values:
densityfilt <- filter(density, density$value > -34 & density$value < 0)

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

WAPnames <- colnames(trainingData[,grep("WAP", names(trainingData))])

rows <- c()

for(i in 1:length(grep("WAP", names(trainingData)))){
  
  s <- which(trainingData[,i] > -29.99 & trainingData[,i] < 0)
  if (length(s) !=0) {
    rows <- c(rows,s)
  }
}

rows <- unique(rows)

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

train_maxsignal_2_4 <- filter(train_maxsignal, train_maxsignal$location == "2-4")

trainingData_2_4 <- filter(trainingData, trainingData$location == "2-4")


trainingData_2_4_us6 <- filter(trainingData, trainingData$location == "2-4" & trainingData$USERID == "6")

WAPus6 <- trainingData_2_4_us6[, grep("WAP", names(validationData))]
trainingData_2_4_us6 <- melt(WAPus6)
trainingData_2_4_us6 <- filter(trainingData_2_4_us6, trainingData_2_4_us6$value != 100)

# ggplot(trainingData_2_4_us6, aes(x=value))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

#### Takeaway: Even though USER 6 has many WIFI signs between -34 and 0, it is still an important USER.

# plot_ly(x = train_maxsignal_2_4$USERID, type = "histogram") # User 3 involved as well?
# plot_ly(x = trainingData_2_4$USERID, type = "histogram") # oNLY 3 USERS have been in 2-4


## We see that the User 6 has uncommon dBs signals. Let´s delve into all the WAP signals of this user:

user6 <- filter(trainingData, trainingData$USERID == 6)


densUser6 <- melt(user6[, grep("WAP|location", names(user6))])

# plot_ly(x = densUser6$location, y= densUser6$value,  type = "bar")

# ggplot(densUser6, aes(x=value))+
  # geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


user6wap_loc <- melt(user6[, grep("WAP|location", names(user6))])
user6wap_loc_r <- filter(user6wap_loc, user6wap$value < 0 & user6wap$value > -34)
# plot_ly(x = user6wap_loc_r$location, y= user6wap_loc_r$value,  type = "bar")


user6wap087_melt <- filter(densUser6, densUser6$variable == "WAP087")

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

#Normalizing the data
# trainingData[,c(1:520)] <- trainingData[,c(1:520)]/104
# validationData[,c(1:520)] <- validationData[,c(1:520)]/104




### TOTAL DATA ###
totaldata <- gdata::combine(trainingData, validationData)

which(apply(totaldata[,c(1:520)], 2, var) == 0) # Combining the Train and Validation datasets there are NOT columns with 0 variance


#### GATHERING ALL THE DATASET IN ORDER TO GLANCE THE DISTRIBUTION OF THE DATA BETWEEN THE BUILDINGS AND FLOORS ###


# plot_ly(totaldata, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
#         colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
#   add_markers() %>%
#   layout(scene= list(xaxis = list(title = "Longitude"),
#                        yaxis = list(title = "Latitude"),
#                       zaxis = list(title = "Floor")))


rm(totaldata)

### Changing variable types and names:

trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)

levels(trainingData$FLOOR)[1] <- 'Floor_0'
levels(trainingData$FLOOR)[2] <- 'Floor_1'
levels(trainingData$FLOOR)[3] <- 'Floor_2'
levels(trainingData$FLOOR)[4] <- 'Floor_3'
levels(trainingData$FLOOR)[5] <- 'Floor_4'

levels(trainingData$BUILDINGID)[1] <- 'Building_0'
levels(trainingData$BUILDINGID)[2] <- 'Building_1'
levels(trainingData$BUILDINGID)[3] <- 'Building_2'

validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)

levels(validationData$FLOOR)[1] <- 'Floor_0'
levels(validationData$FLOOR)[2] <- 'Floor_1'
levels(validationData$FLOOR)[3] <- 'Floor_2'
levels(validationData$FLOOR)[4] <- 'Floor_3'
levels(validationData$FLOOR)[5] <- 'Floor_4'

levels(validationData$BUILDINGID)[1] <- 'Building_0'
levels(validationData$BUILDINGID)[2] <- 'Building_1'
levels(validationData$BUILDINGID)[3] <- 'Building_2'

trainingData$LATITUDE <- as.numeric(trainingData$LATITUDE)
trainingData$LONGITUDE <- as.numeric(trainingData$LONGITUDE)

validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)


### There are 8 conection sets from ValidationData that can be use in the trainset, as these locations are now shown in the TrainingData

valid2_4 <- filter(validationData, validationData$location == "2-4")

addtrain <- valid2_4[c(26, 25, 21, 39, 17, 20, 6, 31),]

trainingData <- rbind(trainingData, addtrain)

validationData <- anti_join(validationData, addtrain)

rm(valid2_4, addtrain)

totaldata <- gdata::combine(trainingData, validationData)

### Saving the datasets:

saveRDS(trainingData,"Data/trainingData.rds")
saveRDS(validationData,"Data/validationData.rds")
saveRDS(totaldata,"Data/totaldata.rds")


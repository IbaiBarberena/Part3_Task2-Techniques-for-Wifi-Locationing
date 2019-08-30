pacman::p_load(tibble, readr, tidyr, anytime, reshape2, corrplot, ggplot2, caret, highcharter,
               plotly, dplyr,plyr,imager,RColorBrewer,gdata,
               randomForest, tidyr, forecast, lubridate, scatterplot3d)

setwd("C:/Users/Ibai/Desktop/Part_3/Task_2/Techniques for Wifi Locationing/")

trainingData <- as_tibble(read.csv2("Data/trainingData.csv", sep= ",", stringsAsFactors=FALSE))
validationData <- as_tibble(read.csv2("Data/validationData.csv", sep= ",", stringsAsFactors=FALSE))

## Pre-processing the data ##

anyNA(trainingData)
anyNA(validationData)


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

trainingData <- unique(trainingData)
validationData <- unique(validationData)

## Plotting the density of the WAP
## Train dataset
WAPdens <- trainingData[, grep("WAP", names(trainingData))]

density <- melt(WAPdens)

ggplot(density, aes(x=density$value))+
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# boxplot
boxplot <- filter(density, density$value != 100)
boxplot(boxplot$value)
rm(boxplot)

## Validation dataset
WAPdens2 <- validationData[, grep("WAP", names(validationData))]

density2 <- melt(WAPdens2)

dev.off()
ggplot(density2, aes(x=density2$value))+
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


# boxplot
boxplot2 <- filter(density2, density2$value != 100)
boxplot(boxplot2$value)

range(boxplot2$value)

rm(boxplot2)

### We have dBm signals that are higher than -34, which is very unusual.
# Analyzing which WAP connections have these values:
densityfilt <- filter(density, density$value > -34 & density$value < 0)

plot_ly(x = densityfilt$variable, type = "histogram")

#Most of the values btween -34 and 0 dBm come from WAP087, WAP065, WAP066
# ggplot(trainingData, aes(x=WAP087))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# 
# ggplot(trainingData, aes(x=WAP065))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)
# 
# ggplot(trainingData, aes(x=WAP066))+
#   geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

WAPnames <- colnames(trainingData[,c(1:520)])

rows <- c()

for(i in 1:520){
  
  s <- which(trainingData[,i] > -34 & trainingData[,i] < 0)
  if (length(s) !=0) {
    rows <- c(rows,s)
  }
}

rows <- unique(rows)

train_maxsignal <- trainingData[rows,]

plot_ly(x = train_maxsignal$PHONEID, type = "histogram")
plot_ly(x = train_maxsignal$location, type = "histogram")
plot_ly(x = train_maxsignal$TIMESTAMP, type = "histogram")
plot_ly(x = train_maxsignal$USERID, type = "histogram")
plot_ly(x = train_maxsignal$RELATIVEPOSITION, type = "histogram")
plot_ly(x = train_maxsignal$SPACEID, type = "histogram")

## We see that the User 6 has uncommon dBs signals. Let´s delve into all the WAP signals of this user:

user6 <- filter(trainingData, trainingData$USERID == 6)

densUser6 <- melt(user6[, grep("WAP", names(user6))])

ggplot(densUser6, aes(x=value))+
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

boxplot3 <- filter(densUser6, densUser6$value != 100)
boxplot(boxplot3$value)

plot_ly(x = user6$TIMESTAMP, type = "histogram")
# We see that the WAP signals of User 6 are splitted in two: Before 10:am and after 11:00
user6_10 <- filter(user6, user6$TIMESTAMP < "2013-06-20 10:30:00")
user6_11 <- filter(user6, user6$TIMESTAMP > "2013-06-20 10:30:00")

ggplot(melt(user6_10[, grep("WAP", names(user6_10))]), aes(x=value))+
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)

ggplot(melt(user6_11[, grep("WAP", names(user6_11))]), aes(x=value))+
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


user6wap2 <- melt(user6_10[, grep("WAP", names(user6_10))])
user6wap2 <- filter(user6wap2, user6wap2$value < 0 & user6wap2$value > -34)
plot_ly(x = user6wap2$variable, y= user6wap2$value,  type = "bar")


user6wap3 <- melt(user6_11[, grep("WAP", names(user6_11))])
user6wap3 <- filter(user6wap3, user6wap3$value < 0 & user6wap3$value > -34)
plot_ly(x = user6wap3$variable, y= user6wap3$value,  type = "bar")


user6wap <- melt(user6[, grep("WAP", names(user6))])
user6wap_r <- filter(user6wap, user6wap$value < 0 & user6wap$value > -34)
plot_ly(x = user6wap_r$variable, y= user6wap_r$value,  type = "bar")

user6wap087 <- filter(user6wap, user6wap$variable == "WAP087")

user6_087 <- filter(user6wap, user6wap$variable == "WAP087")

ggplot(user6_087, aes(x=user6_087$value)) +
  geom_density(color="darkblue", fill="lightblue") + xlim(-104,-1)


# User 6 has in total 566 RSSI signs between -34 an 0. 13,25% of them come from WAP087. This values range from -10 and -20


##### DEALING WITH ALL THE NO SIGNAL VALUES ########


# trainingData <- replace(trainingData, trainingData[,] == 100, 0)
# 
# 
# for(i in 1:520){
#   
#   if (trainingData[,i] != 0) {
#     trainingData[,i] <- replace(trainingData[,i], trainingData[,i] + 104)
#   }
# }
# 
# trainingData[,c(1:520)] 
# 
# trainingData[,c(1:520)] <- replace(trainingData[,c(1:520)], trainingData[,c(1:520)] != 0, trainingData[,c(1:520)] + 104)


# Deleting all the columns that have 0 values. It means that these WAPs haven't identified any IPSS signal.

totaldata <- gdata::combine(trainingData, validationData)

which(apply(totaldata[,c(1:520)], 2, var) == 0) # Combining the Train and Validation datasets there are not columns with 0 variance





#### GATHERING ALL THE DATASET IN ORDER TO GLANCE THE DISTRIBUTION OF THE DATA BETWEEN THE BUILDINGS AND FLOORS ###


plot_ly(totaldata, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~source,
        colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

count <- totaldata %>%  
  group_by(source, BUILDINGID) %>%  
  summarize(source = count(source),
    building = count(BUILDINGID))

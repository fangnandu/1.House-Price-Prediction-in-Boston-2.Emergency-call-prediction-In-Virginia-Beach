library(dplyr)
library(tidyverse)
library(sf)
library(caret)
library(AppliedPredictiveModeling)
library(pscl)
library(e1071)
#maptheme
pallete4_10_colors <- c("#E8DB4C","#E2B645","#DC923E","#D66D37","#D14931","#AF483E","#8D474B","#6B4758","#494665","#274672")

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_rect("darkgrey"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line("darkgrey"),
    panel.grid.minor = element_line("darkgrey"),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

#1data preparation
#1.1 ARCGIS-data cleannng
#1.1.1cleanning data not in the virginia beach
#choosing the emergency calls from 12-01 00:00:00 to 12-21 23:59:59; then import 
# it into ArcGis, and display its X AND Y coordinate. We will there are 5 points not belongs to the 
# processing extent. 
#1.1.2 spatial join the tract data to the emergency point.
#1.1.3 based on the date of the emergency point, create field for the emergency point which is the 
#count of emergency calls for 3 weeks ,2 weeks and one week. And use the spatial join to do that.
#then we will get the raw emergency data(thefinaldatanewSAT.csv)


#1.2 Using R to get the data prepared
#1.2.1 use the r to extract the day and hour from the raw date
emsdata<- read.csv("thefinaldatanewSAT.csv")
emsdata$Call_Date <-mdy_hms(emsdata$Call_Date)
emsdata$day<-day(emsdata$Call_Date)
emsdata$hour<-hour(emsdata$Call_Date)
#1.2.2 group the emsdata by TRACT,DAY AND HOUR
#count the number of emergency happened in that specific day and hour
emsdata<- emsdata %>% 
  group_by(TRACT,day, hour)  %>% 
  summarize(
    X...callID = n()
  )
colnames(emsdata)=c("TRACT","day","hour","callnumber")


#1.2.3 construct the time series for 3 weeks and merge to get the fulldataset
hourseries<-data.frame(hour=seq(0:23)-1)
dayforthreeweek<-data.frame(day=seq(1:21))
thetimeseries<- merge(hourseries,dayforthreeweek,all=TRUE)
# construct the tract data(only contains the tract value)
tract<-read.csv("tractnewfinal.csv")
thefulldataseries<- merge(thetimeseries,tract,all=TRUE)
colnames(thefulldataseries)=c("hour","day","TRACT")
#merge the timeseries and the cleaned data to get the full data( every day every hour every tract)
#our research group will be tract and we will based on it.
thefulldata <- merge(thefulldataseries ,emsdata,by.x=c("TRACT","day","hour"),by.y=c("TRACT","day","hour"),all.x =TRUE)

#1.2.4 merge with tract (demographic characteristic)
#merge the tract information into the emsdata 
tractdata<- read.csv("thetractdata.csv")
thedatainR <- merge(thefulldata ,tractdata,by.x=c("TRACT"),by.y=c("TRACT"),all.x =TRUE)

#1.3 add the weather data into the dataset
#problem and the answer
#problem: After having runned the script below to get the weather, i notice there are only 503 rows.which means 
# there is a missing data for 1 hour. 
#Answer: I located the missing data the 2016-12-07 00:00:00 , so i add the data by estimating based on the neighbor's value
#Then i extract the weather data from 2016-12-01 to 2016 12-06 to name it weather 1
# 1.3.1 get the weather from 2016-12-01 to 2016-12-06
library(doParallel)
library(foreach)
library(lubridate)
weather.data <- data.frame(date=seq(ymd_hms("2016-12-01 00:00:00"),ymd_hms("2016-12-06 23:59:59"),by="hour"))
cl <- makeCluster(2)
registerDoParallel(cl)
results <- foreach(i=0:7,.packages="lubridate") %dopar%{
  # paste together the URL
  url.text <- paste("http://www.wunderground.com/history/airport/KNTU/",
                    year(weather.data$date[i*24+1]),"/",
                    month(weather.data$date[i*24+1]),"/",
                    day(weather.data$date[i*24+1]),"/",
                    "DailyHistory.html",
                    sep="")
  # read in the webpage
  a <- scan(url.text,what="",sep="\n")
  
  # search for hourly data
  i <- grep("56 (A|P)M</td>",a)
  
  # temperature
  temp <- as.numeric(gsub("(<[^>]*>|&[^;]*;|[ F])","",a[i+2]))
  
  # precipitation
  preci <- as.numeric(gsub("(<[^>]*>|&[^;]*;|[ in]|\\t)","",a[i+25]))
  # NA to 0
  preci[is.na(preci)] <- 0
  
  return(cbind(temp,preci))
}
results <- do.call(rbind,results)
stopCluster(cl)

weather.data$temperature <- results[,1]
weather.data$precipitation <- results[,2]
write.csv(weather.data, "weatherfirst7days.csv")

# 1.3.2 get the weather from 2016-12-07 01:00:00 to 2016-12-21 23:59:59
weather.data <- data.frame(date=seq(ymd_hms("2016-12-07 01:00:00"),ymd_hms("2016-12-21 23:59:59"),by="hour"))
cl <- makeCluster(2)
registerDoParallel(cl)
results <- foreach(i=0:15,.packages="lubridate") %dopar%{
  # paste together the URL
  url.text <- paste("http://www.wunderground.com/history/airport/KNTU/",
                    year(weather.data$date[i*24+1]),"/",
                    month(weather.data$date[i*24+1]),"/",
                    day(weather.data$date[i*24+1]),"/",
                    "DailyHistory.html",
                    sep="")
  # read in the webpage
  a <- scan(url.text,what="",sep="\n")
  
  # search for hourly data
  i <- grep("56 (A|P)M</td>",a)
  
  # temperature
  temp <- as.numeric(gsub("(<[^>]*>|&[^;]*;|[ F])","",a[i+2]))
  
  # precipitation
  preci <- as.numeric(gsub("(<[^>]*>|&[^;]*;|[ in]|\\t)","",a[i+25]))
  # NA to 0
  preci[is.na(preci)] <- 0
  
  return(cbind(temp,preci))
}
results <- do.call(rbind,results)
stopCluster(cl)

weather.data$temperature <- results[,1]
weather.data$precipitation <- results[,2]
write.csv(weather.data, "weatherlast14days.csv")

#1.3.3 using the excel to add the temperature for the missing data (2016-12-07 00:00:00) and load the weather data
weather<- read.csv("weatherfinal.csv")
weather<-weather[,c(-1)]
names(weather)
thedatainR<-merge(thedatainR,weather,by.x=c("day","hour"),by.y=c("day","hour"))

#1.4 add the time lag into the dataset
thedatainR$lag.hour<-lag(thedatainR$callnumber,1)
#make the NA into o
thedatainR[is.na(thedatainR)] <- 0


# 2 regression
#2.1 before we do the regression, i want to make some visulization of the emergency calls
# to better understand the spatial distribution of it
#2.1.1 visualize the emergency calls for different time period
CensusMap <- st_read("tract.shp")

# the actuall calls for 2 weeks 
ggplot() + 
  geom_sf(data=CensusMap, aes(fill=CensusMap$Count_2), colour= "white")+
  labs(title= "Actuall Calls for 2 weeks")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,20),
                       name = "Actuall calls")+
  mapTheme()
# the actuall calls for 1 weeks 
ggplot() + 
  geom_sf(data=CensusMap, aes(fill=CensusMap$Count_1), colour= "white")+
  labs(title= "Actuall Calls for 1 weeks")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,20),
                       name = "actuall calls")+
  mapTheme()

# the actuall calls for 3 weeks 
ggplot() + 
  geom_sf(data=CensusMap, aes(fill=CensusMap$Count_), colour= "white")+
  labs(title= "Actuall Calls for 3 weeks")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,20),
                       name = "actuall calls")+
  mapTheme()


#2.2 regression model
#2.2.1  regression model
#now we have the regression data(thedatainR), we will use this data to do the regression.
#first, mutate the category variables into factor
thedatainR<-
  thedatainR %>%
  mutate(hour = as.factor(hour),
         TRACT = as.factor(TRACT)) %>%
  as.data.frame() 
#run the regression and summarize it

ggplot(data = thedatainR,aes(callnumber))+geom_histogram(binwidth = 1)
#From the distribution of the dependent variable, we would like to choose the poisson regression in this project
#regression try model
reginR<-glm(thedatainR$callnumber  ~.,family = "poisson",data = thedatainR %>% dplyr::select(-TRACT,-day))
summary(reginR)
#regression final model
reginRfinal<-glm(thedatainR$callnumber  ~.,family = "poisson",data = thedatainR %>% dplyr::select(-day))
summary(reginRfinal)
reg<-reginRfinal
#2.2.2 Standardized coefficients
library(tidyverse)
library(sf)
library(QuantPsyc)
standardized <- as.data.frame(lm.beta(reg))
standardized$variable <- row.names(standardized)
colnames(standardized)[1] <- "std_coefficient"
standardized
#plot this
ggplot(standardized, aes(x=variable, y=std_coefficient, fill=variable)) + 
  geom_bar(stat="identity")

#Which variable is most important? I???m not sure it matters if the coefficient is negative or positive. Let???s take the absolute value.
ggplot(standardized, aes(x=variable, y=abs(std_coefficient), fill=variable)) + 
  geom_bar(stat="identity")
#Better. How about we reorder by magnitude. First, create a permanant absolute valued standardized coefficient variable.
standardized$absCoef <- abs(standardized$std_coefficient)

ggplot(standardized, aes(x=reorder(variable,-absCoef), y=absCoef, fill=variable)) + 
  geom_bar(stat="identity")

# 3 validation 
#In validation, i select the first two week as the trainning data and the last one week as the test data
train<-thedatainR[ which(thedatainR$day <14),]
test<-thedatainR[ which(thedatainR$day >13),]
reginRv<-glm(callnumber  ~.,family = "poisson",data = train %>% dplyr::select(-day))
summary(reginRv)

#observed and predicted
pred2 <- predict(reginRv, test) 
#plot the predicted emergency calls as a funtion of observed emergency calls
names(pred2)
head(pred2)
regDF2 <- cbind(test$callnumber, pred2)
colnames(regDF2) <- c("Observed", "Predicted")
regDF2 <- as.data.frame(regDF2)
testPred<-cbind(test,pred2)
ggplot() + 
  geom_point(data=regDF2, aes(Observed, Predicted)) +
  stat_smooth(data=regDF2, aes(Observed, Observed), method = "lm", se = FALSE, size = 1) + 
  labs(title="Predicted  emergecy calls as a function\nof Observed emergency calls") +
  theme(plot.title = element_text(size = 18,colour = "black"))
# the MAE
mean(abs(pred2-test$callnumber))

# 4 visualization
#4.1 the predicted emergency calls for a specific time
#4.1.1  for the from DEC-12-20-20:00:00  to Dec-12-20-20:59:59

t<-which(testPred$day==20 & testPred$hour == 20)
test20_20<-testPred[t,]
test20_20 <-
  test20_20 %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
CensusMap <-
  CensusMap %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
CensusMapNew20_20 <-
  CensusMap %>%
  left_join(test20_20, by=c("TRACT"))
ggplot() + 
  geom_sf(data=CensusMapNew20_20, aes(fill=pred2), colour= "white")+
  labs(title= "Predicted Calls Dec 20th,  20:00-20:59")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,20),
                       name = "Predicted calls")+
  mapTheme()

#4.1.2  for the from DEC-12-17-22:00:00  to Dec-12-17-22:59:59

t<-which(testPred$day==17 & testPred$hour == 22)
test17_22<-testPred[t,]
test17_22 <-
  test17_22 %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
CensusMapNew17_22 <-
  CensusMap %>%
  left_join(test17_22, by=c("TRACT"))

ggplot() + 
  geom_sf(data=CensusMapNew17_22, aes(fill=pred2), colour= "white")+
  labs(title= "Actual Calls Dec 17th,   22:00-22:59")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,10),
                       name = "Predicted calls")+
  mapTheme()
#4.2 the actuall emergency calls for a specific time
#Actual Calls Dec 20th,   20:00-20:59
ggplot() + 
  geom_sf(data=CensusMapNew20_20, aes(fill=CensusMapNew20_20$callnumber), colour= "white")+
  labs(title= "Actual Calls Dec 20th,   20:00-20:59")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,10),
                       name = "actual calls")+
  mapTheme()
#Actual Calls Dec 17th,   22:00-22:59
ggplot() + 
  geom_sf(data=CensusMapNew17_22, aes(fill=CensusMapNew17_22$callnumber), colour= "white")+
  labs(title= "Actual Calls Dec 17th,   22:00-22:59")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,10),
                       name = "actual calls")+
  mapTheme()
# 4.3 the predicted emergency calls for a specific location for one day
# Predicted EMS: TRACT:045408, TIME: Dec.20th
t<-which(testPred$TRACT==045408 & testPred$day == 20)
test045408_20<-testPred[t,]

test045408_20 <-
  test045408_20 %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
ggplot() + 
  geom_line(data = test045408_20, aes(x = hour, y = pred2), color = "orange",group=1, size = 2) +
  geom_line(data = test045408_20, aes(x = hour, y = test045408_20$callnumber), color = "deepskyblue",group=1, size = 2) +
  labs(x = "Time Period", y = "Number of calls", title = "TRACT:045408, TIME: Dec.20th")
# Predicted: TRACT:041200, TIME: Dec.20th
t<-which(testPred$TRACT==041200 & testPred$day == 20)
test041200_20<-testPred[t,]

test041200_20 <-
  test041200_20 %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
ggplot() + 
  geom_line(data = test045408_20, aes(x = hour, y = pred2), color = "orange",group=1, size = 2) +
  geom_line(data = test045408_20, aes(x = hour, y = test045408_20$callnumber), color = "deepskyblue",group=1, size = 2) +
  labs(x = "Time Period", y = "Number of calls", title = "TRACT:041200, TIME: Dec.20th")


# 5 goodness of model 
# MAE
mean(abs(reginRv$fitted.values - train$callnumber))
mean(abs(pred2-test$callnumber))
#test the goodness of model by specific time 

ggplot() + 
  geom_sf(data=CensusMapNew20_20, aes(fill=(abs(CensusMapNew20_20$callnumber- pred2))), colour= "white")+
  labs(title= "Absolute Prediction Error")+
  scale_fill_gradientn(colors = pallete4_10_colors,         
                       breaks=c(1,20),
                       name = "Absolute Prediction Error")+
  mapTheme()

testPred <-
  testPred %>%
  mutate(
    TRACT = as.numeric(TRACT)) 
reg_Summary <-
  testPred %>%
  group_by(TRACT) %>%
  summarise(MAE = mean(abs(callnumber - pred2), na.rm=TRUE)) %>%
  mutate(Legend="reg") %>%
  left_join(CensusMap, by=c("TRACT")) %>%
  st_sf()

ggplot() + 
  geom_sf(data=CensusMap, aes(), fill=NA, colour="white") +
  geom_sf(data=reg_Summary, aes(fill=MAE), colour="darkgrey") +
  labs(title="Mean Absolute Error in each area in Virginia Beach") +
  scale_fill_gradient(low ="white" ,high ="darkorange" )+
  mapTheme()

write.csv (x = as.data.frame(testPred), file = "testPred.csv")


#Goodness of fit :Risk Terrain vs kernel density hotspot
#Goodness of Fit test.

#Move your testPred.csv into arcgis.
# #Join it to the tract shapefile
# Use polygon to raster to create a raster called fitted. 
# Reclassify fitted to run 1-10. Call this raster fitted_rcl.
# Reclassify fitted_rcl into a 5 group raster called fitted_rcl2. My breaks are as follows:
#   90 to 100 = Class 1 70 to 89 = Class 2 50 to 69 = Class 3 30 to 49 = Class 4 1 to 29 = Class 5
# 
# Use raster to polygon to convert fitted_rcl2 into polygons. Call this fittedGroups.shp
# Spatial join the emergency points to the fittedGroups.shp. Call this fittedgroups_joinPoints.shp.
# Summarize the groups to get count of emergency by group. In your new .dbf, create a new field called fittedCnt and set it equal to the count of emergency
# Now onto the kernel density comparison.
# 
# Run kernel density. 
# Reclass this to run 1- 10. Call this raster kernel_rcl.
# Reclass again into the same 5 classes you used above, Call this kernel_rcl2. Note that you can pull up your former classifications from the Results tab.
# Use raster to polygon to convert kernel_rcl2 into polygons. Call this kernelGroups.shp
# Spatial join the emergency points to the kernelGroups.shp. Call this kernelGroups_joinPoints.shp.
# Summarize the groups to get count of emergency by group. In your new .dbf, create a new field called kernelCnt and set it equal to the count of emergency.
# Now to make the comparisons.
# 
# Join the kernel density summarized dbf with the comparable dbf from the risk terrain model. Join on the category, 1-5, GRIDCODE. Export as countComparisons.txt
# Import countComparisons.txt into R.
# Select out the category field as well as the two fields that describe the count of emergency by both rtm and kernel density.
# Add a field that changes the category field, GRIDCODE into a sting-based field denoting the percentage range of predictions.
# Create new fields that represent the count of emergency by category as a percent of all the emergency.
countComparisons <- read.csv("countComparisons.csv")

countComparisons <- 
  countComparisons %>%
  dplyr::select(X...GRIDCODE, kernelCnt, fittedCnt)

countComparisons <- cbind(
  countComparisons,
  data.frame(Category = c("90% - 100%", "70% - 89%", "50% - 69%", "30% - 49%", "1% - 29%")))

countComparisons <- 
  countComparisons %>%
  dplyr::mutate(kernelPct = kernelCnt / sum(kernelCnt),
                fittedPct = fittedCnt / sum(fittedCnt))

countComparisons

countComparisonsLong <-
  countComparisons %>% 
  gather(Variable, Value, kernelPct:fittedPct)

ggplot(data=countComparisonsLong, aes(Category,Value)) +
  geom_bar(aes(fill = Variable), position = "dodge", stat="identity") +
  scale_fill_manual(values = c("darkgreen", "blue"),
                    labels=c("Risk Terrain", "Kernel Density")) +
  labs(x= "Predicted Risk Levels",
       y="Percent of correctly predicted cases",
       title= "Goodness of Fit: Risk Terrain vs. Kernel Density hotspot") 
# We did the risk terrain model to compare with the kernel density.
# So as we can see, our model is good at the probability of emergency calls over 90% and below 50%.
# In that, our model will can predict high probability for emergency calls.
# Overall, the goodness of fit is good for the prediction. 
# But there are some limitations about this final project, which is the observations are limited 
# to 3 weeks. For the emergency calls, the dataset is too small. Hope we can expand the time 
# period to have larger dataset and better prediction.


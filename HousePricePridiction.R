#1.1 Data wrangling- setup
library(corrplot)
library(caret) 
library(AppliedPredictiveModeling)
library(stargazer)
library(ggmap)
library(tidyverse)
library(sf)
library(FNN)
# 1.1.1 set the map them
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}


#1.1.2 add new variables (using r to calculate the near distance)
#1.1.2.1the distance to education
biz <- st_read("Boston_Midterms_Dataset.shp") 
bostonSalesnew <- st_read("Boston_Midterms_Dataset.shp") 
educationfinal <- st_read("education_final.shp")
#check the projection
st_crs(bostonSalesnew) == st_crs(educationfinal)

homeprice <- st_read("Boston_Midterms_Dataset.shp") %>%
  select(geometry) %>%
  st_transform(4326) %>%
  mutate(landUse = "homeprice")

education <- st_read("education_final.shp") %>%
  select(geometry) %>%
  st_transform(4326) %>%
  mutate(landUse = "education")
allPlaces <- rbind(homeprice, education)
allPlaces <- cbind(as.data.frame(st_coordinates(allPlaces)), data.frame(allPlaces))
#visualize
pghBaseMap <- get_map(location = "boston, Massachusetts", 
                      source = "stamen", 
                      zoom = 11, 
                      maptype= 'toner')
ggmap(pghBaseMap) + 
  geom_point(data = allPlaces, 
             aes(x=X, y=Y, color=landUse), 
             size = 0.5) + 
  labs(title="homeprice & education around boston") +
  scale_colour_manual(values = c("darkgreen","red"),
                      name = "Land Use",
                      labels=c("homeprice","education")) +
  mapTheme()
#
homepriceXY <-
  allPlaces %>%
  filter(landUse == "homeprice") %>%
  select(X,Y) %>%
  as.matrix()   

educationXY <-
  allPlaces %>%
  filter(landUse == "education") %>%
  select(X,Y) %>%
  as.matrix() 
nn = get.knnx(educationXY,homepriceXY,k=5)
names(nn)
# add the variable( distance to education) to the raw data
biz2 <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "home_prices") %>%
  gather(education, education_Distance, V1:V5) %>%
  arrange(as.numeric(home_prices)) %>%
  group_by(home_prices) %>%
  summarize(d_education = mean(education_Distance)) %>%
  arrange(as.numeric(home_prices)) %>% 
  select(-home_prices) %>%
  bind_cols(biz)
#visualize the distance

ggmap(pghBaseMap) + 
  geom_point(data = biz2, 
             aes(x=Longitude, y=Latitude, color=factor(ntile(d_education,5))), 
             size = 2) + 
  geom_point(data = subset(allPlaces, landUse == "education"), 
             aes(x=X, y=Y), colour = "red") + 
  labs(title="Distance from homeprice to 5 nearest education",
       subtitle = "education in red") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(biz2$d_education,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="distance\n(in thousands)\n (Quintile Breaks)") +
  mapTheme()

names(biz2)


#1.1.2.2 add the distance to transportation point 
trans <- st_read("boston_massachusetts_osm_transport_points.shp")
st_crs(trans) == st_crs(bostonSalesnew)
homeprice <- st_read("Boston_Midterms_Dataset.shp") %>%
  select(geometry) %>%
  st_transform(4326) %>%
  mutate(landUse = "homeprice")

trans <- st_read("boston_massachusetts_osm_transport_points.shp") %>%
  select(geometry) %>%
  st_transform(4326) %>%
  mutate(landUse = "transportation")

allPlaces <- rbind(homeprice, trans)
allPlaces <- cbind(as.data.frame(st_coordinates(allPlaces)), data.frame(allPlaces))
pghBaseMap <- get_map(location = "boston, Massachusetts", 
                      source = "stamen", 
                      zoom = 11, 
                      maptype= 'toner')
ggmap(pghBaseMap) + 
  geom_point(data = allPlaces, 
             aes(x=X, y=Y, color=landUse), 
             size = 0.1) + 
  labs(title="homeprice & transportation point around boston") +
  scale_colour_manual(values = c("darkgreen","red"),
                      name = "Land Use",
                      labels=c("homeprice","transportation")) +
  mapTheme()

homepriceXY <-
  allPlaces %>%
  filter(landUse == "homeprice") %>%
  select(X,Y) %>%
  as.matrix()   

transXY <-
  allPlaces %>%
  filter(landUse == "transportation") %>%
  select(X,Y) %>%
  as.matrix() 

nn = get.knnx(transXY,homepriceXY,k=5)

names(nn)

biz3 <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "home_prices") %>%
  gather(trans, trans_Distance, V1:V5) %>%
  arrange(as.numeric(home_prices)) %>%
  group_by(home_prices) %>%
  summarize(d_trans = mean(trans_Distance)) %>%
  arrange(as.numeric(home_prices)) %>% 
  select(-home_prices) %>%
  bind_cols(biz2)
#visualize

ggmap(pghBaseMap) + 
  geom_point(data = biz3, 
             aes(x=Longitude, y=Latitude, color=factor(ntile(d_trans,5))), 
             size = 1)  + 
  labs(title="Distance from homeprice to 5 nearest transportation point",
       subtitle = "transportation point in red") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(biz3$d_trans,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="distance\n(in thousands)\n (Quintile Breaks)") +
  mapTheme()

#1.1.3 write the combined data to xlsx
library(xlsx)
write.xlsx(newdata, "~/Desktop/mid-pro")
write.table(newdata, file="newdata.csv",sep=",",row.names=F)


#1.1.4 using gis to do the density process to get 3 more variables and add these 3
# variables to the combined data. 
#1.1.4.1 crime density visualiztion

CRIME<- st_read("crime.shp")

ggmap(pghBaseMap) + 
  geom_point(data = CRIME, 
             aes(x=Longitude, y=Latitude, color=factor(ntile(CRIME$RASTERVALU,5))), 
             size = 1)  + 
  labs(title="Crime density in Boston") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(CRIME$RASTERVALU,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="crime density\n(in thousands)\n (Quintile Breaks)") +
  mapTheme()
# 1.1.4.2 road_density
road_dens<- st_read("road_densfinal.shp")

ggmap(pghBaseMap) + 
  geom_point(data = road_dens, 
             aes(x=Longitude, y=Latitude, color=factor(ntile(road_dens$RASTERVALU,5))), 
             size = 1)  + 
  labs(title="road density in Boston") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(road_dens$RASTERVALU,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="road density\n(in thousands)\n (Quintile Breaks)") +
  mapTheme()


#the all variables are added and name it thefianlregressiondata



#1.1.5 add the spatial dimension data
# select the variables
#read the shapefiles and csv to load the data
bostonSales <- st_read("Boston_Midterms_Dataset.shp") 
bostonNhoods <- st_read("Boston_Neighborhoods.shp")
bostontract<- st_read("Census2010_Tracts.shp")

st_crs(bostonSales) == st_crs(bostonNhoods)
bostontract <- st_transform(bostontract, st_crs(bostonSales))
st_crs(bostonSales) == st_crs(bostontract)
bostonSales_joinNhoods <- st_join(bostonSales, bostonNhoods)
bostonSales_joinNhoods <- st_join(bostonSales_joinNhoods, bostontract)
install.packages("readxl")
library(readxl)
finalregressiondata <- read_excel("finalregressiondata.xlsx")
locationfactor<-bostonSales_joinNhoods[,c(7,9,12,15,17:19,22:41,60:63,65,73)]
newdatafinal<-bind_cols(locationfactor,finalregressiondata)
try1<-newdatafinal[,c(1:34,36,46,68:73)]
names(try1)
unlist(lapply(try1, class))

#the dataset has been cleaned and also all variables are in the try1. Now,we can run the regression




#1.2 data wrangling-explantory analysis
baseMap <- get_map(location = "boston", 
                   source = "stamen", 
                   zoom = 11, 
                   maptype= 'toner')

ggmap(baseMap) + 
  geom_point(data = try1, 
             aes(x=Longitude, y=Latitude, color=factor(ntile(SalePrice,5))), 
             size = 1) + 
  labs(title="saleprice, boston") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(try1$SalePrice,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="salepricee\nin thousands\n (Quintile Breaks)") +
  mapTheme()

#summary of the statistics
#select the numeric 
unlist(lapply(try1, class))
numericdata<-try1 [,c(10,13,14,15,20:24,35,36:38,40,41)]
#delete the  geometry
numericdata <-
  numericdata %>%
  as.data.frame() %>%
  select(-geometry) %>%
  na.omit()
#add the saleprice on the numericdata
saleprice<-try1 [,c(34)]
saleprice <-
  saleprice %>%
  as.data.frame() %>%
  select(-geometry) %>%
  na.omit()
numericdata<-bind_cols(saleprice,numericdata)
unlist(lapply(numericdata, class))


#statistic summary
stargazer(numericdata, type="text", title = "Summary Statistics")
# plot the correlation map
M <- cor(numericdata)
bizCor <- numericdata
unlist(lapply(bizCor, class))
M <- cor(bizCor)
corrplot(M, method = "number")



#2.1: Modeling - In-sample prediction

#seperate the trainning dataset and the test dataset
try1 <-
  try1 %>%
  mutate(ZIPCODE = as.factor(ZIPCODE),
         PTYPE = as.factor(PTYPE),
         R_FPLACE= as.factor(R_FPLACE),
         YR_REMOD = as.numeric(YR_REMOD)) %>%
  as.data.frame() %>%
  filter(test == 0)%>%
na.omit()
#run the regression
reg <- lm(log(SalePrice) ~  ZIPCODE+PTYPE+OWN_OCC+LAND_SF+YR_REMOD1+GROSS_AREA
           +LIVING_ARE+NUM_FLOORS+R_ROOF_TYP+R_EXT_FIN+
             R_TOTAL_RM+R_BDRMS+R_FULL_BTH+R_HALF_BTH+R_KITCH+
             highway_dis+road_dens+crime_dens+d_trans+d_education+Name, 
           data = try1) 

summary(reg)

#1 new add*********************************
names(reg)
regPredValues <- 
  data.frame(observedSales = try1$SalePrice,
             predictedSales = exp(reg$fitted.values))

regPredValues <-
  regPredValues %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) 

head(regPredValues)
mean(regPredValues$absError)
mean(regPredValues$percentAbsError)
##********************



# visulize the predicted value and the observed value

reg$fitted.values<-exp(reg$fitted.values)
options(scripen=3)
regDF <- cbind(try1$SalePrice, reg$fitted.values)
colnames(regDF) <- c("Observed", "Predicted")
regDF <- as.data.frame(regDF)
options(scripen=3)
ggplot() + 
  geom_point(data=regDF, aes(Observed, Predicted)) +
  stat_smooth(data=regDF, aes(Observed, Observed), method = "lm", se = FALSE, size = 1) + 
  labs(title="Predicted Sales Volume as a function\nof Observed Sales Volume") +
  theme(plot.title = element_text(size = 18,colour = "black"))

#histgram of residual
hist(abs(try1$SalePrice - reg$fitted.values), breaks=50, main="Histrogram of residuals (absolute values)")


#residual map
reg_residuals <- data.frame(reg$residuals)
bosLonLat <- data.frame(try1$Longitude, try1$Latitude)
residualsToMap <- cbind(bosLonLat, reg_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")

invert <- function(x) rgb(t(255-col2rgb(x))/255)    
baseMap_invert <- as.raster(apply(baseMap, 2, invert))
class(baseMap_invert) <- class(baseMap)
attr(baseMap_invert, "bb") <- attr(baseMap, "bb")
ggmap(baseMap_invert)+
  mapTheme()


ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = 0.1) + 
  labs(title="Residual of reg, boston") +
  scale_color_gradient(low="green", high="pink")


#moran's I
install.packages('spdep')
library(spdep)
#moran's i
coords <- cbind(residualsToMap$longitude, residualsToMap$latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg$residuals, nb2listw(spatialWeights, style="W"))


#2.2 Modeling 
inTrain <- createDataPartition(
  y = try1$SalePrice, 
  p = .75, list = FALSE)
training <- try1[ inTrain,] #the new training set
test <- try1[-inTrain,]  #the new test set
nrow(training) / nrow(try1)
reg2  <- lm(log(SalePrice) ~  ZIPCODE+PTYPE+OWN_OCC+LAND_SF+YR_REMOD1+GROSS_AREA
                     +LIVING_ARE+NUM_FLOORS+R_ROOF_TYP+R_EXT_FIN+
                       R_TOTAL_RM+R_BDRMS+R_FULL_BTH+R_HALF_BTH+R_KITCH+
                       highway_dis+road_dens+crime_dens+d_trans+d_education+Name, 
                     data = try1) 
reg2Pred <- predict(reg2, test) 
#plot
names(reg2Pred)
reg2Pred<-exp(reg2Pred)
head(reg2Pred)
regDF2 <- cbind(test$SalePrice, reg2Pred)
colnames(regDF2) <- c("Observed", "Predicted")
regDF2 <- as.data.frame(regDF2)
ggplot() + 
  geom_point(data=regDF2, aes(Observed, Predicted)) +
  stat_smooth(data=regDF2, aes(Observed, Observed), method = "lm", se = FALSE, size = 1) + 
  labs(title="Predicted Sales Volume as a function\nof Observed Sales Volume") +
  theme(plot.title = element_text(size = 18,colour = "black"))

#new fit metrics
reg2PredValues <- 
  data.frame(observedSales = test$SalePrice,
             predictedSales = reg2Pred)

reg2PredValues <-
  reg2PredValues %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) 

head(reg2PredValues)
mean(reg2PredValues$absError)
mean(reg2PredValues$percentAbsError)

#3 new add**************residual as a function of the observation 25% data


ggplot() + 
  geom_point(data=reg2PredValues, aes(observedSales, error)) +
  labs(title="Residual as a function\nof Observed Sales ") +
  theme(plot.title = element_text(size = 18,colour = "black"))
#3 new add**************residual as a function of the predicted 25% data

ggplot() + 
  geom_point(data=reg2PredValues, aes(predictedSales, error)) +
  labs(title="Residual as a function\nof predicted Sales ") +
  theme(plot.title = element_text(size = 18,colour = "black"))

#new added MAPE BY NEIGHBERHOOD
library(tidyverse)
library(sf)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

bostonSales <- st_read("Boston_Midterms_Dataset.shp") 
bostonNhoods <- st_read("Boston_neighborhoods.shp")
st_crs(bostonSales) == st_crs(bostonNhoods)
bostonNhoods <- st_transform(bostonNhoods, st_crs(bostonSales))
bostonSales_joinNhoods <- st_join(bostonSales, bostonNhoods)
vars <- bostonSales_joinNhoods[,c(6,9,18,25:40,61:63,65)]
library(dplyr)
vars <-
  vars %>%
  mutate(YR_BUILT = as.factor(YR_BUILT),
         NUM_FLOORS = as.factor(NUM_FLOORS),
         YR_REMOD = as.factor(YR_REMOD)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  filter(test == 0) %>%
  na.omit()

devtools::install_github("rstudio/ggvis", build_vignettes = FALSE)
reg2Residuals <- 
  data.frame(residuals = exp(reg2$residuals),
             SalePrice = vars %>% select(SalePrice),
             Longitude = vars %>% select(Longitude),
             Latitude = vars %>% select(Latitude),
             Name = vars %>% select(Name),
             Legend = "reg2")

reg2Residuals_Summary <-
  reg2Residuals %>%
  group_by(Name) %>%
  summarize(meanResidual = mean(residuals, na.rm=TRUE),
            sdResidual = sd(residuals, na.rm=TRUE),
            mean(abs(exp(reg2$fitted.values) - vars$SalePrice) / vars$SalePrice),
            countSales = n()) %>%
  mutate(Legend="reg2") %>%
  mutate(mean="mean(abs(exp(reg2$fitted.values) - vars$SalePrice) / vars$SalePrice)") %>%
  filter(countSales > 5) %>%
  left_join(bostonNhoods) %>%
  st_sf()

mean(abs(exp(reg2$fitted.values) - vars$SalePrice) / vars$SalePrice)


ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill=NA, colour="black", size=1) +
  geom_point(data=reg2Residuals, 
             aes(Longitude,Latitude, color=factor(ntile(residuals,5))),size=1) +
  
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                      labels=as.character(quantile(reg2Residuals_Summary$`mean(abs(exp(reg2$fitted.values) - vars$SalePrice)/vars$SalePrice)`,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Residuals \n(Quintile Breaks)") +
  mapTheme()





#2.3 model cross_validation
fitControl <- trainControl(method = "cv", number = 10)

set.seed(825)

lmFit <- train(log(SalePrice) ~  ZIPCODE+PTYPE+OWN_OCC+LAND_SF+YR_REMOD1+GROSS_AREA
               +LIVING_ARE+NUM_FLOORS+R_ROOF_TYP+R_EXT_FIN+
                 R_TOTAL_RM+R_BDRMS+R_FULL_BTH+R_HALF_BTH+R_KITCH+
                 highway_dis+road_dens+crime_dens+d_trans+d_education+Name, 
               data = try1,
               method = "lm", 
               trControl = fitControl)
lmFit
lmFit$resample
sd(lmFit$resample[,3])

ggplot(as.data.frame(lmFit$resample), aes(MAE)) + 
  geom_histogram(bins=4) +
  labs(x="Mean Absolute Error",
       y="Count")

#2 new add************
ggplot(as.data.frame(lmFit$resample), aes(Rsquared)) + 
  geom_histogram(bins=4) +
  labs(x="Rsquared",
       y="Count")
#*********************


# 4predict the value
trynew2<-newdatafinal[,c(1:34,35,36,46,68:73)]
trynew<-try
testdata<-trynew2
testdata <-
  testdata %>%
  mutate(ZIPCODE = as.factor(ZIPCODE),
         PTYPE = as.factor(PTYPE)) %>%
  as.data.frame() %>%
  filter(test == 1)


summary(reg)
prefinal1 <- predict(reg, testdata) 
prefinal1<-exp(prefinal1)

new<-bind_cols(prefinal1,testdata)

regPredValues <- 
  data.frame(UniqueSale = testdata$UniqueSale,
             predictedSales = prefinal1)
summary(reg)
prefinal1 <- predict(reg, testdata) 
prefinal1<-exp(prefinal1)


regPredValues <- 
  data.frame(UniqueSale = testdata$UniqueSale,
             predictedSales = prefinal1)



write.csv(regPredValues, 'finalpredictionyep.csv')


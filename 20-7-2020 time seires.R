library(tseries)
library (lubridate)
library(ggplot2)
library(dplyr)
library(zoo)
library(xts)
library(ggplot2)
library (stats)
library(forecast)
library(readxl)
library(tsbox)
library(randtests)# for run test
install.packages("randtests")
install.packages("tsbox")



#data that we will be used. 

library(readxl)
FMDforts <- read_excel("FMDforts.xlsx")
View(FMDforts)

#### test randomness by run tet #### https://rdrr.io/cran/randtests/man/runs.test.html
runs.test(FMDforts$district, alternative = "two.sided", plot = TRUE)

#time series with number of case
tsFMD2<-ts(FMDforts$case, frequency=12, start=c(2011,1))
autoplot(tsFMD)

#time series with number of subdistrict
tsFMDdistrict2<-ts(FMDforts$district, frequency=12, start=c(2011,1))
autoplot(tsFMDdistrict2)

### try autoregression model ######
pacf(tsFMDdistrict2, lag = 12) #https://online.stat.psu.edu/stat501/lesson/14/14.1#:~:text=An%20autoregressive%20model%20is%20when,from%20that%20same%20time%20series.&text=In%20this%20regression%20model%2C%20the,a%20simple%20linear%20regression%20model.
#The PACF is most useful for identifying the order of an autoregressive model. Specifically, sample partial autocorrelations that are significantly different from 0 indicate lagged terms of y that are useful predictors of yt
lag1case<- lag(tsFMDdistrict2, -1)
lag9case<- lag(tsFMDdistrict2, -9)
plot(tsFMDdistrict2~ lag1case+lag9case, xy.labels=F)

lagdata <- ts.intersect(tsFMDdistrict2, lag1case, lag9case, dframe=T)
summary(lm(tsFMDdistrict2 ~ lag1case+lag9case, data=lagdata))

### use stl function #####

stltssubdistrict<-stl(tsFMDdistrict2,s.window = "periodic") # use stl function
plot(stltssubdistrict)
apply(stltssubdistrict$time.series,2,var)/var(tsFMDdistrict2) # compare variance of each components with original series
summary(stltssubdistrict)
ggAcf(tsFMDdistrict2, lag = 12)
gglagplot(tsFMDdistrict2)

##### seasonal plot ########
ggseasonplot(tsFMD2, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("no. of cases") +
  ggtitle("Seasonal plot: Number of FMD cases") # seasonal plot

ggseasonplot(tsFMDdistrict2, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("no. of subdistrict") +
  ggtitle("Seasonal plot: Number of subdistrict") # seasonal plot

ggseasonplot(tsFMD2, polar=TRUE) +
  ylab("no. of cases") +
  ggtitle("Polar seasonal plot: Number of FMD cases") # polar seasonal plot

ggseasonplot(tsFMDdistrict2, polar=TRUE) +
  ylab("no. of subdistrict") +
  ggtitle("Polar seasonal plot: Number of subdistrict") # polar seasonal plot

ggsubseriesplot(tsFMD2) +
  ylab("no. of cases") +
  ggtitle("Seasonal subseries plot: Number of FMD cases")

ggsubseriesplot(tsFMDdistrict2) +
  ylab("no. of subdistrict") +
  ggtitle("Seasonal subseries plot: Number of subdistrict")


#use regression model
summary(tslm(tsFMDdistrict2 ~  trend+season))
summary(tslm(tsFMD2 ~ trend+season))

#use polynomial regression model
summary(tslm(tsFMDdistrict2 ~  trend+I(trend^2)+season))
summary(tslm(tsFMD2 ~ trend+I(trend^2)+season))

train_tsFMDdistrict2 <- window(tsFMDdistrict2, start=2011, end=2017) # select 2011 to 2017 for train set
plot(forecast(tslm(train_tsFMDdistrict2 ~  trend+I(trend^2)+season), h=12))
autoplot(tsFMDdistrict2) # polynomial regression model is not accurate from what I see



autoplot(stl(tsFMD2, t.window=13, s.window="periodic", robust=TRUE)) 
autoplot(stl(tsFMDdistrict2, s.window="periodic", robust=TRUE))
#https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/forecast.stl

summary(stl(tsFMD2, t.window=13, s.window="periodic", robust=TRUE))


cumsum(FMDforts$case[FMDforts$Year==2011])
plot(FMDforts$month[FMDforts$Year==2011], cumsum(FMDforts$district[FMDforts$Year==2011]))
plot(FMDforts$month[FMDforts$Year==2012], cumsum(FMDforts$district[FMDforts$Year==2012]))
plot(FMDforts$month[FMDforts$Year==2013], cumsum(FMDforts$district[FMDforts$Year==2013]))
plot(FMDforts$month[FMDforts$Year==2014], cumsum(FMDforts$district[FMDforts$Year==2014]))
plot(FMDforts$month[FMDforts$Year==2015], cumsum(FMDforts$district[FMDforts$Year==2015]))
plot(FMDforts$month[FMDforts$Year==2016], cumsum(FMDforts$district[FMDforts$Year==2016]))
plot(FMDforts$month[FMDforts$Year==2017], cumsum(FMDforts$district[FMDforts$Year==2017]))
plot(FMDforts$month[FMDforts$Year==2018], cumsum(FMDforts$district[FMDforts$Year==2018]))

mean(FMDforts$district)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
FMDforts$cum_district<-ave(FMDforts$district,FMDforts$Year,FUN=cumsum) #create column with cumulative district

FMDforts$normcum_district<-ave(FMDforts$cum_district,FMDforts$Year,FUN=normalize)#normalize cumulative district


FMDforts$Year<-as.factor(FMDforts$Year)
ggplot(data = FMDforts, aes(x=month, y=normcum_district))+ geom_line(aes(colour=Year))+geom_smooth(method = "lowess", size = 1.5) +  scale_y_continuous(name="Normalised outbreaks", limits=c(0, 1))+scale_x_continuous(labels = function(x) month.abb[x],breaks=seq(1,12,1))
#https://stackoverflow.com/questions/58209870/months-on-x-axis-instead-of-number for x-axis as month
str(FMDforts)

###################################################################################
############# barplot timeseries ##################################################
data.tsFMDdistrict2<-ts_df(tsFMDdistrict2 ) # change time series to dataframe
data.tsFMDdistrict2$mean3<-c(rep(NA,5),rollmean(data.tsFMDdistrict2$value,12)) # create moving average column
  
ggplot(data.tsFMDdistrict2, aes(x=time, y=value)) + geom_bar(stat="identity")

ggplot(data.tsFMDdistrict2, aes(x=time)) + 
  geom_line(aes(y = value)) +  xlab("Year")+scale_x_date(date_breaks = "1 year",date_labels = "%Y")+ylab("Monthly FMD Outbreak incidences")

####################################################################################
########### try fit model with linear model ########################################
lag1case<- lag(tsFMDdistrict2, -1)
lagdata <- ts.intersect(tsFMDdistrict2, lag1case, dframe=T)
data.tsFMDdistrict2<-ts_df(tsFMDdistrict2 ) #change ts to dataframe
data.tsFMDdistrict2$month<-rep(1:12,8)
data.tsFMDdistrict2$trend<-c(1:96)
data.tsFMDdistrict2$year<-rep(1:8,each=12)
data.tsFMDdistrict2$lag1<-c(NA,lagdata$lag1case)
#data.tsFMDdistrict2$month<-as.factor(data.tsFMDdistrict2$month)
#data.tsFMDdistrict2$year<-as.factor(data.tsFMDdistrict2$year)
summary(data.tsFMDdistrict2)

summary(lm(value ~lag1+ year+trend+I(trend^2), data=data.tsFMDdistrict2))
summary(lm(value ~ lag1+year+I(year^2)+sin(2*pi*month/12)+ cos(2*pi*month/12), data=data.tsFMDdistrict2))
summary(lm(value ~ lag1+year+I(year^2)+month+I(month^2), data=data.tsFMDdistrict2))

rcorr(as.matrix(data.tsFMDdistrict2[,c(2:5)]), type = c("spearman"))

#sum outbreak each year
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==1])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==2])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==3])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==4])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==5])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==6])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==7])
sum(data.tsFMDdistrict2$value[data.tsFMDdistrict2$year==8])
mean(data.tsFMDdistrict2$value)
sd(data.tsFMDdistrict2$value)

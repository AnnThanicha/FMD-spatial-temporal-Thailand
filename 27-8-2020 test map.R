library(sf)
library(DCluster)
library(spdep)
library(ggplot2)
library(rgdal)
library(dplyr)
library(maptools)

install.packages("rgdal")
install.packages ("maptools")


mapsmr <- st_read("SMR.shp") # read shape file
summary(mapsmr)
mapsmr<-mapsmr[!is.na(mapsmr$case2011),]

nb <- poly2nb(mapsmr , queen=TRUE)# make neighbour weight matrix
?poly2nb
lw <- nb2listw(nb, style="W", zero.policy=TRUE) # assign weight to neigbour

nb2013 <- poly2nb(mapsmr[mapsmr$pop2013>0,] , queen=TRUE) # create neighbor weight matrix with only pop >0 polygon
lw2013 <- nb2listw(nb2013, style="W", zero.policy=TRUE)

nb2015 <- poly2nb(mapsmr[mapsmr$pop2015>0,] , queen=TRUE) # create neighbor weight matrix with only pop >0 polygon
lw2015 <- nb2listw(nb2015, style="W", zero.policy=TRUE)

nb2018 <- poly2nb(mapsmr[mapsmr$pop2018>0,] , queen=TRUE) # create neighbor weight matrix with only pop >0 polygon
lw2018 <- nb2listw(nb2018, style="W", zero.policy=TRUE)

moran.test(mapsmr$EB2011,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2012,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2013,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2014,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2015,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2016,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2017,lw, zero.policy=TRUE, na.action=na.omit)
moran.test(mapsmr$EB2018,lw, zero.policy=TRUE, na.action=na.omit)

?moran.test


dat<-data.frame(Observed= mapsmr$case2011, Expected=mapsmr$e2011, Population = mapsmr$pop2013, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2012, Expected=mapsmr$e2012, Population = mapsmr$pop2013, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2013, Expected=mapsmr$e2013, Population = mapsmr$pop2013, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2014, Expected=mapsmr$e2014, Population = mapsmr$pop2015, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2015, Expected=mapsmr$e2015, Population = mapsmr$pop2015, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2016, Expected=mapsmr$e2016, Population = mapsmr$pop2015, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2017, Expected=mapsmr$e2017, Population = mapsmr$pop2018, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster
dat<-data.frame(Observed= mapsmr$case2018, Expected=mapsmr$e2018, Population = mapsmr$pop2018, x=mapsmr$degx, y= mapsmr$degy) # make dataframe use for Dcluster



dat<-dat[dat$Population>0, ]# remove pop =0 because it can't identify

achisq.test(Observed~offset(log(Expected)),data = dat, model = "poisson" , R= 999)
?achisq.test

moranI.test(Observed~offset(log(Expected)),data = dat, model = "negbin" , listw = lw2018, 999,  n = length(nb2018),S0 = Szero(lw2018),zero.policy=TRUE)




opgam(data = dat, radius = 1000, step = 10, alpha = 0.002)


tango.stat(data=dat, listw=lw, zero.policy=TRUE)
tango.test(Observed~offset(log(Expected)),data = dat, model = "multinom", listw = lw,zero.policy=TRUE, R = 999)


 mle <- calculate.mle(as(dat, "data.frame"), model = "negbin")
 thegrid <- as(dat, "data.frame")[, c("x", "y")]
###################################################
 ### calculate EB lognormal ##########
 smr <- readRDS("C:/Users/chanc004/OneDrive - WageningenUR/Phd thesis/R code/Objective1 Spaial analysis/mapsmr.rds")
 summary(smr)
 
 smr2011to2013<-smr[smr$pop2013>0,]# remove area with pop = 0
 smr2014to2016<-smr[smr$pop2015>0,]# remove area with pop = 0
 smr2017to2018<-smr[smr$pop2018>0,]# remove area with pop = 0
 
 smr2011to2013$EB2011<-exp(lognormalEB(smr2011to2013$case2011, smr2011to2013$e2011)$smthrr)
 smr2011to2013$EB2012<-exp(lognormalEB(smr2011to2013$case2012, smr2011to2013$e2012)$smthrr)
 smr2011to2013$EB2013<-exp(lognormalEB(smr2011to2013$case2013, smr2011to2013$e2013)$smthrr)
 
 smr2014to2016$EB2014<-exp(lognormalEB(smr2014to2016$case2014, smr2014to2016$e2014)$smthrr)
 smr2014to2016$EB2015<-exp(lognormalEB(smr2014to2016$case2015, smr2014to2016$e2015)$smthrr)
 smr2014to2016$EB2016<-exp(lognormalEB(smr2014to2016$case2016, smr2014to2016$e2016)$smthrr)
 
 smr2017to2018$EB2017<-exp(lognormalEB(smr2017to2018$case2017, smr2017to2018$e2017)$smthrr)
 smr2017to2018$EB2018<-exp(lognormalEB(smr2017to2018$case2018, smr2017to2018$e2018)$smthrr)
 
 summary(smr$EB2011); summary(smr$smr2011)
 
 summary(empbaysmooth(smr$case2011,smr$e2011)$smthrr)
 
 write.csv(smr2011to2013, "smr2011to2013.csv", row.names =TRUE) # save to csv to join in qgis

 write.csv(smr2014to2016, "smr2014to2016.csv", row.names =TRUE)
 write.csv(smr2017to2018, "smr2017to2018.csv", row.names =TRUE) 
 
 
 ################ count frequency SMR ###################
mapsmr$Tumbol_ID<-as.character(mapsmr$Tumbol_ID)
  hsm2011<-mapsmr$Tumbol_ID[mapsmr$smr2011>1]# find tumbol ID which smr>=1
 hsm2012<-mapsmr$Tumbol_ID[mapsmr$smr2012>1]
 hsm2013<-mapsmr$Tumbol_ID[mapsmr$smr2013>1]
 hsm2014<-mapsmr$Tumbol_ID[mapsmr$smr2014>1]
 hsm2015<-mapsmr$Tumbol_ID[mapsmr$smr2015>1]
 hsm2016<-mapsmr$Tumbol_ID[mapsmr$smr2016>1]
 hsm2017<-mapsmr$Tumbol_ID[mapsmr$smr2017>1]
 hsm2018<-mapsmr$Tumbol_ID[mapsmr$smr2018>1]

summary
 hsm<-cbind(hsm2011,hsm2012,hsm2013,hsm2014,hsm2015,hsm2016,hsm2017,hsm2018)
 write.csv(hsm, "hsm.csv", row.names =TRUE) # exort as csv to count frequency in excel
 ?aggregate

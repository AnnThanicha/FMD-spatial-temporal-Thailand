install.packages("gee")
install.packagFes("geepack")
install.packages("car")
install.packages("PerformanceAnalytics")



library(gee)
library(geepack)
library(car)
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)

FMD2011_2018caseused <- read_excel("FMD2011_2018caseused.xlsx")
FMD2011_2018TID <- read_excel("FMD2011_2018TID.xlsx")

saveRDS(FMDdat8,"FMDdat8.rds")
tumbol_distance <- readRDS("tumbol_distance.rds")
checkFMDcase_map <- readRDS("checkFMDcase_map.rds")


#geepack need binary outcome as numeric
FMDdat6$FMDstatus.x<-as.numeric(as.character(FMDdat6$FMDstatus.x))
FMDdat7<- FMDdat6[ -c(1:8, 10,11) ] # delete unwant column
FMDdat7$month<-as.factor(FMDdat7$month)#change month year to factor
FMDdat7$year<-as.factor(FMDdat7$year)
str(FMDdat7)
FMDdat7$factor.FMDstatus<-as.factor(FMDdat7$FMDstatus.x)# create column with FMD status as factor
summary(FMDdat7)

FMDdat8<-FMDdat7[order(FMDdat7$Tumbol_ID, FMDdat7$year, FMDdat7$month),]
#FMDdat7[13:27][is.na(FMDdat7[13:27])] <- 0 # replace farm size NA with 0
summary(FMDdat9$Distance.x)
############## check distance variable ##################
a<-FMDdat8

a$lag<-paste0(a$year,"-",a$month) # check which month and year without distance
a$lag<-as.factor(a$lag)
summary(a[is.na(a$Distance.x),])#check which month did not have distance
a<-a[is.na(a$Distance.x),]# select data with distance = NA

a$month<-(as.numeric(as.character(a$month))) # change month from factor to nummeric
a$month_1<-a$month-1 # delete -1 to make it to previous month
a$year<-(as.numeric(as.character(a$year))) #change month, year to numeric



a$month_1<-as.character(a$month_1) # change to character to change 2011-08 to 2011-07
a$month_1[a$year ==2011 ] <- revalue(a$month_1[a$year ==2011 ], c("8"="7"))
a$month_1<-as.numeric(a$month_1)

b<-left_join(a,FMD2011_2018caseused[c("Tumbol_ID","year","month")],by=c("month_1"="month", "year"="year")) #join case and subdistrict

b$Tumbol_ID.y<-as.character(b$Tumbol_ID.y)
tumbol_distance$Tumbol_ID<-as.character(tumbol_distance$Tumbol_ID)
tumbol_distance$Outbreak_ID<-as.character(tumbol_distance$Outbreak_ID)
#join distance
b<-left_join(b, tumbol_distance, by = c("Tumbol_ID.x"="Tumbol_ID", "Tumbol_ID.y"="Outbreak_ID")) #join distance

#aggreagate min distance
str(b)
d<-aggregate(Distance~month+year+Tumbol_ID.x, data = b, min)

summary(d)

g<-FMDdat8


d$month<-as.character(d$month)
d$year<-as.character(d$year)
gnew<-left_join(g,d, by = c("Tumbol_ID"="Tumbol_ID.x", "year"="year", "month"="month_1") )

summary(gnew)


gnew$Distance.x <- ifelse(is.na(gnew$Distance.x), gnew$Distance, gnew$Distance.x)#replaceNA with distance 
summary(gnew)
gnew[,c("Distance")] <- list(NULL)#delete distance
FMDdat9<-gnew #save gnew as FMDdat9
summary(FMDdat9)
FMDdat9[order(FMDdat9$Tumbol_ID,FMDdat9$year,FMDdat9$month),]#sort data by Tumbol_Id month year


saveRDS(FMDdat9, "FMDdat9.rds")

############# correlation ####################
summary(FMDdat8)
corre1<-round(cor(FMDdat8[c(8:27)]),2)
corre1<-data.frame(corre1)
write.csv(corre1,"corre1.csv")

###### check descriptive ##############################
#for categorical variable
TC<-function(a){
mytable <- table(a,FMDdat8$factor.FMDstatus)
per_table<-prop.table(mytable, 2) #colun percentage
chi<-chisq.test(FMDdat8$factor.FMDstatus, a)
result<-list(mytable,per_table,chi) 
return(result)
}


TC(a=FMDdat8$border)
TC(a=FMDdat8$month)

#for continous variable
summary(FMDdat8[c(8:27,43)])
sd(FMDdat8$beef[FMDdat8$factor.FMDstatus==0])
sd(FMDdat8$beef[FMDdat8$factor.FMDstatus==1])
str(FMDdat8)
aggregate(smallruminant   ~  factor.FMDstatus, data=FMDdat9, summary)
aggregate(. ~  factor.FMDstatus , data=FMDdat8[c(2:4,43)], summary)
aggregate(. ~  factor.FMDstatus, data=FMDdat8[c(8:27,43)],summary)
aggregate(rain ~  month, data=FMDdat7, sd)
aggregate(rain ~  year, data=FMDdat7, mean)

mean(FMDdat8$beef_large[FMDdat8$factor.FMDstatus==1],na.rm = TRUE)
max(FMDdat8$beef_large[FMDdat8$factor.FMDstatus==1],na.rm = TRUE)
?mean
chart.Correlation(FMDdat7[c(8:27)], histogram=TRUE, pch=19)
?geeglm

summary(FMDdat6)

############ model select correlation structure #####################
geeModel1_1<-geeglm(FMDstatus.x ~1,data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="ar1")
summary(geeModel1_1)
QIC(geeModel1_1)
rm(geeModel1_1)
?geeglm
geeModel1_2<-geeglm(FMDstatus.x ~1,data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="independence")
summary(geeModel1_2)
QIC(geeModel1_2)
rm(geeModel1_2)

geeModel1_3<-geeglm(FMDstatus.x ~1,data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel1_3)
QIC(geeModel1_3)
rm(geeModel1_3)
gc()


################# univariable analysis ##############################################
str(a)
summary(a$factor_percent_buffalosmall)
#for factor
geeModel_1<-geeglm(FMDstatus.x ~relevel(border , ref = "Combodia"),data=test,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_1)
exp(coef(geeModel_1))
QIC(geeModel_1)
rm(geeModel_1)



str(a)
#for incomplete case
geeModel_1<-geeglm(FMDstatus.x ~factorpercent_smallruminantsmall,data=a3[!is.na(a3$factorpercent_dairysmall ),],family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_1)
exp(coef(geeModel_1))
QIC(geeModel_1)
anova(geeModel_1)
rm(geeModel_1)

geeModel_1<-geeglm(FMDstatus.x ~factorpercent_smallruminantlarge,data=a3[!is.na(a3$factorpercent_dairysmall ),],family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_1)
exp(coef(geeModel_1))
QIC(geeModel_1)
anova(geeModel_1)
rm(geeModel_1)

#for continuous
str(a)
geeModel_1<-geeglm(FMDstatus.x ~ log(total_smallruminantfarm+1)  ,data=a[!is.na(a$total_smallruminantfarm ),],family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_1)
exp(coef(geeModel_1))
QIC(geeModel_1)
rm(geeModel_1)

summary(a$dairy)
summary(a$logdairy)

memory.limit()
memory.limit(1.76e+13)

#####################################################################
#### check month with sine-cosine function ##############################

a$nummonth<-as.numeric(as.character(a$month)) # change month to numeric
summary(a$nummonth)
geeModel_1<-geeglm(FMDstatus.x ~ sin(2*pi*nummonth/12) + cos(2*pi*nummonth/12) ,data=test,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_1)
exp(coef(geeModel_1))
QIC(geeModel_1)
rm(geeModel_1)

############################################################################
################# relevel before run multivariable analysis ################
a <- within(a, border <- relevel(border, ref = "Combodia"))

summary(a)
#####################################################################
###### multivariable analysis ##########################
str(a)
 
geeModel_full1<- geeglm(FMDstatus.x ~ factor_elevate +year+factor_rain+border+neighbor+market+slaughter+logdistance+factor_perforest+logdairy+logbeef+logbuffalo+logpig+logsmallruminant+factor_percent_beefmedium+factor_percent_beeflarge+factor_percent_beefsmall+ factor_percent_smallruminantlarge+factor_percent_smallruminantmedium+ factor_percent_smallruminantsmall+factor_percent_pigsmall+factor_percent_pigmedium+factor_percent_piglarge+factor_percent_dairymedium+factor_percent_dairysmall +factor_percent_dairylarge +factor_percent_buffalosmall+factor_percent_buffalomedium+factor_percent_buffalolarge +sin(2*pi*nummonth/12)+ cos(2*pi*nummonth/12)  ,
                        data=a[!is.na(a$factor_percent_smallruminantsmall ),],family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_full1)
exp(coef(geeModel_full1))
QIC(geeModel_full1)
#rm(geeModel_full1)

geeModel_full1<- geeglm(FMDstatus.x ~ factor_elevate +year+factor_rain+border+neighbor+market+slaughter+logdistance+factor_perforest+logbeef+logbuffalo+logpig+logsmallruminant+factor_percent_beefmedium+factor_percent_beeflarge+factor_percent_beefsmall+factor_percent_dairymedium+factor_percent_dairysmall +factor_percent_dairylarge+sin(2*pi*nummonth/12)+ cos(2*pi*nummonth/12)  ,
                        data=a[!is.na(a$factor_percent_smallruminantsmall ),],family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_full1)
exp(coef(geeModel_full1))
QIC(geeModel_full1)

geeModel_full0.1 <- update(geeModel_full1, . ~ .- logbeef)
summary(geeModel_full0.1)
exp(coef(geeModel_full0.1))
QIC(geeModel_full0.1)
rm(geeModel_full0.1)


geeModel_full2 <- geeglm(FMDstatus.x ~ factor_elevate +factor_perforest+year+factor_rain+border+neighbor+market+slaughter+logdistance+logdairy+logbeef+logbuffalo+logpig+logsmallruminant+sin(2*pi*nummonth/12)+ cos(2*pi*nummonth/12)  ,
                         data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_full2)
exp(coef(geeModel_full2))
QIC(geeModel_full2)

geeModel_full2.1 <- geeglm(FMDstatus.x ~ factor_elevate +year+factor_rain+border+neighbor+logdistance+logdairy+logbuffalo+logpig+logbeef+logsmallruminant+sin(2*pi*nummonth/12)+ cos(2*pi*nummonth/12)  ,
                         data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_full2.1)
exp(coef(geeModel_full2.1))
QIC(geeModel_full2.1)


geeModel_full2.2 <- geeglm(FMDstatus.x ~ factor_elevate +year+factor_rain+border+neighbor+logdistance+logdairy+market+logbuffalo+logpig+logbeef+logsmallruminant+sin(2*pi*nummonth/12)+ cos(2*pi*nummonth/12)  ,
                           data=a,family=binomial(link = "logit"),id=Tumbol_ID,corstr="exchangeable")
summary(geeModel_full2.2)
exp(coef(geeModel_full2.2))
QIC(geeModel_full2.2)

### function for calcualte confidence interval ###
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-mult*Std.err,
                      upr=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

confint.geeglm(geeModel_full2)

# look like there is a correlation between the log dairy and border
summary(a$logdairy[a$border == "Myanmar"])
summary(a$logdairy[a$border == "Loas"])
summary(a$logdairy[a$border == "Malaysia"])
summary(a$logdairy[a$border == "Combodia"])
summary(a$logdairy[a$border == "Nocontact"])
########################################################################

saveRDS(a,"a.rds")# reserve a 
#calculate extreme value using sine-cosine function by Stolwijk, 1999
atan(0.009822/0.06929)*(12/(2*pi))#beta1/beta2>0
0.269+12/2 # extreme value = t, t+T/2
atan(0.03647/(-0.04525))*(6/(2*pi))#beta1/beta2<0 if beta>0 the first value is maximum
-0.648+6/2;  # extreme value = t+T/2, t+T
(-0.648) + 6 ; # because it is six month period then the value for next maximum and minimum is in next 6 months

#so for the result from our beta sine-cosine function
atan( 0.588963/ 1.823399 )*(12/(2*pi))
0.597+12/2 # so the maximum =  January and minimum = July

#for both sine-cosine function
atan( 0.009869 / 0.06985 )*(12/(2*pi));(0.268+12/2)
atan( 0.03661 /0.04511  )*(6/(2*pi));(0.651+6/2)

atan(-0.22931  /(  0.55205))*(12/(2*pi))#beta1/beta2<0 
-0.7518991+12/2;  # extreme value = t+T/2, t+T
(-0.7518991) + 12 #for beta1<0 the first beta is minimum, if beta1>0 the first beta is maximum
#so maximum =  November, minimum = May 

atan(-0.3347    /( 0.4927  ))*(12/(2*pi))#beta1/beta2<0 
-1.14+12/2;  # extreme value = t+T/2, t+T
( -1.14) + 12 #for beta1<0 the first beta is minimum, if beta1>0 the first beta is maximum
#so maximum =  November, minimum = May 
?geeglm

atan( (0.009869+ 0.06985)/ (0.03661+0.04511))*(12/(2*pi));(0.268+12/2)


rm(geeModel_full0.1)

summary(a$border);summary(a$FMDstatus.x) 
summary(a_new$border);summary(a_new$FMDstatus.x) 



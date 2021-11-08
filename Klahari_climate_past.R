rm(list=ls(all=TRUE))


### Paniw et al. XXXXX

### R script to visualize temperature and rainfall trends in the KALAHARI (1979-2020)

# load necessary packages


library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

library(scales)

CV <- function(x){
  (sd(x,na.rm = T)/mean(x,na.rm = T))*100
}


setwd("...")


## RAINFALL

rainfall=read.csv("rainfall_GPCP_1979_2020.csv") # average daily rainfall

rainfall$date=as.Date(rainfall$date) 
rainfall$month=month(rainfall$date)
rainfall$year=year(rainfall$date)

rain.sub=rainfall[rainfall$lon==21.25&rainfall$lat==(-26.25),] # retain rainfall values for the appropriate coordinates
rain.sub=rain.sub[rain.sub$date<"2021-01-01",] 

for(i in 1:nrow(rain.sub)){
  
  rain.sub$rain[i]=30.4*rain.sub$rain[i]
  
}

## MONTHLY RAINFALL
rain.sub$month=factor(rain.sub$month)

b1=ggplot(data=rain.sub,aes(date,rain))+
  geom_line(col="blue")+
  theme_bw(base_size = 22)+
  xlab("Time") +
  geom_smooth(method="lm",aes(date,rain,group=month),col="black",alpha=0.1)+
  scale_color_viridis_d(name="",option="D")+
  ylab("Total monthly rainfall (mm)")+
  theme(panel.grid = element_blank())+
  theme(axis.text.y = element_text(color="black"))+
  theme(axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=0.9),
        legend.position = "top",
        legend.key.size = unit(3,"line"))+
  theme(plot.margin = unit(c(0.2,0.1,0.5,0.1), "cm"))+
  scale_x_date(date_breaks = "48 month",date_minor_breaks = "12 month", labels = date_format("%Y"), expand = c(0, 0))+
  theme(axis.text.x = element_text(color="black"))

b1

summary(glm(rain~year,data=rain.sub[rain.sub$month%in%1,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%2,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%3,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%4,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%5,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%6,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%7,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%8,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%9,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%10,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%11,]))
summary(glm(rain~year,data=rain.sub[rain.sub$month%in%12,]))


## VARIATION IN ANNUAL RAINFALL
var.rain=aggregate(rain~year,CV, data=rain.sub)

b2=ggplot(data=var.rain,aes(year,rain))+
  geom_line(col="blue")+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank())+
  xlab("") +
  geom_smooth(method="lm",col="black")+
  scale_color_viridis_d(name="",option="B",begin=0,end=0.8)+
  ylab("CV rainfall")+
  theme(plot.margin = unit(c(0,0.01,0.0,0.01), "cm"))+
  theme(axis.text = element_text(color="black"))

b2

summary(lm(rain~year,data=var.rain)) # not a significant trend in CV

## TOTAL ANNUAL RAINFALL

sum.rain=aggregate(rain~year,sum, data=rain.sub)

b3=ggplot(data=sum.rain,aes(year,rain))+
  geom_line(col="blue")+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank())+
  xlab("") +
  geom_smooth(method="lm",col="black")+
  scale_color_viridis_d(name="",option="B",begin=0,end=0.8)+
  ylab("Total annual rainfall")+
  theme(plot.margin = unit(c(0,0.01,0.0,0.01), "cm"))+
  theme(axis.text = element_text(color="black"))

b3


summary(lm(rain~year,data=sum.rain)) # slight increase in rain, but not signifcant 


## MAXIMUM TEMPERATURE

temperature=read.csv("tempNOAA_CPC_1979_2020.csv") # average daily maximum temperature

temperature$date=as.Date(temperature$date) 
temperature$month=month(temperature$date)
temperature$year=year(temperature$date)

sub=temperature
start=as.Date("02/15/1979",format="%m/%d/%Y")
end=as.Date("12/15/2020",format="%m/%d/%Y")

dates=seq(start, end , by="month")

tempMAX=data.frame(temp=rep(NA,length(dates)),
                   date=rep(NA,length(dates)))

for(i in 1:length(dates)){
  
  
  tempMAX$temp[i]=mean(sub$temp[sub$date<=dates[i]&sub$date>(dates[i]-months(1))],na.rm=T) # mean maximum temperature
  tempMAX$date[i]=as.character(dates[i])
}

tempMAX$date=as.Date(tempMAX$date)
tempMAX$year=year(tempMAX$date)
tempMAX$month=month(tempMAX$date)


tempMAX$month=factor(tempMAX$month)

a1=ggplot(data=tempMAX,aes(date,temp))+
  geom_line(col="orange")+
  # facet_wrap(~scen)+
  theme_bw(base_size = 22)+
  xlab("Time") +
  geom_smooth(method="lm",aes(date,temp,group=month),col="black",alpha=0.1)+
  scale_color_viridis_d(name="",option="D")+
  ylab("Mean monthly maximum temperature (ºC)")+
  theme(panel.grid = element_blank())+
  theme(axis.text.y = element_text(color="black"))+
  theme(axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=0.9),
        legend.position = "top",
        legend.key.size = unit(3,"line"))+
  theme(plot.margin = unit(c(0.2,0.1,0.5,0.1), "cm"))+
  scale_x_date(date_breaks = "48 month",date_minor_breaks = "12 month", labels = date_format("%Y"), expand = c(0, 0))+
  theme(axis.text.x = element_text(color="black"))

a1

tempMAX$year2=as.numeric(as.factor(tempMAX$year))

### NOTE: Significance mean +/- 95 CI (1.96*se) does not cross 0

summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%1,])) # signifcant 
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%2,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%3,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%4,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%5,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%6,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%7,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%8,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%9,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%10,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%11,])) # signifcant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%12,])) # signifcant


### PRIOR TO 2013 (where temperature rise has exalerated)

summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%1&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%2&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%3&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%4&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%5&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%6&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%7&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%8&tempMAX$year<2013,]))
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%9&tempMAX$year<2013,]))  # significant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%10&tempMAX$year<2013,])) # significant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%11&tempMAX$year<2013,])) # significant
summary(glm(temp~year2,data=tempMAX[tempMAX$month%in%12&tempMAX$year<2013,])) # significant

#### Separate plots for significant months: 

sub=tempMAX[tempMAX$month%in%c("9","10","11","12")&tempMAX$year<2013,]

sub$pred=NA

sub$se=NA

sub$pred[sub$month%in%"9"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%9&tempMAX$year<2013,]))
sub$se[sub$month%in%"9"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%9&tempMAX$year<2013,]),se.fit = T)$se.fit


sub$pred[sub$month%in%"10"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%10&tempMAX$year<2013,]))
sub$se[sub$month%in%"10"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%10&tempMAX$year<2013,]),se.fit = T)$se.fit


sub$pred[sub$month%in%"11"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%11&tempMAX$year<2013,]))
sub$se[sub$month%in%"11"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%11&tempMAX$year<2013,]),se.fit = T)$se.fit


sub$pred[sub$month%in%"12"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%12&tempMAX$year<2013,]))
sub$se[sub$month%in%"12"]=predict(glm(temp~year2,data=tempMAX[tempMAX$month%in%12&tempMAX$year<2013,]),se.fit = T)$se.fit


ggplot(data=sub,aes(year,temp))+
  geom_point(col="orange",size=1.05)+
  geom_line(aes(year,pred),size=1.05)+
  geom_ribbon(aes(x=year,y=pred,ymin=pred-1.96*se,ymax=pred+1.96*se),fill="grey",alpha=0.4)+
  theme_bw(base_size = 24)+
  facet_grid(~month)+
  xlab("") +
  ylab(expression(T[max]~(ºC)))+
  theme(panel.grid = element_blank())+
  theme(axis.text = element_text(color="black"))


mean.temp=aggregate(temp~year,mean, data=tempMAX)

a=ggplot(data=mean.temp,aes(year,temp))+
  geom_line(col="orange",size=1.05)+
  theme_bw(base_size = 24)+
  xlab("") +
  geom_smooth(method="gam",col="black",size=1,alpha=0.2)+
  ylab(expression(T[max]~(ºC)))+
  theme(panel.grid = element_blank())+
  theme(axis.text.y = element_text(color="black"))+
  theme(plot.margin = unit(c(0,0.01,0,0.01), "cm"))+
  theme(axis.text.x = element_blank())

a

summary(glm(temp~year+I(year^2),data=mean.temp))

# Prior to 2013
summary(glm(temp~year,data=mean.temp[mean.temp$year<2013,]))


### Paniw et al. XXXXX

### R script to construct and project meerkat IBM

## R version 4.0.0

# Code developed by Maria Paniw

# load necessary packages

library(MASS)
library(dplyr)
library(optimx)
library(lubridate)

options(warn=2) # end warnings in error to make sure IBM does not return bogus results 

setwd("...") # set path to location where all files necessary to run the script are

# load initial group conditions

lh=read.csv("init.group.comp.csv")

# load past rain and temperature deviations

past.clim=read.csv("past.rain.temp.csv")

# load mass and age of dominant immigrant males

age.mass.domM=read.csv("age.mass.domM.csv")

# Set maximum newborn and adult mass to prevent outliers (only an issue when initial group sizes are very small and stochastic mortality high, which may create unstable simulations)

max.mass.pup=5.983+0.12
max.mass.pupTB=5.308+0.12 # pup mass is lower when group shows TB symptoms - account for that

max.mass.adult=6.882+0.12

# load GAMs (from https://github.com/MariaPaniw/IBM_meerkat )

file.names <- list.files(path = "./GAMs")

for(i in 1:length(file.names)){
  
  load(paste("./GAMs/",file.names[i],sep=""))
}

### INITIALIZE

ibm.data=NULL

parameters=1:1000 # #number of simulations 

par.sub=parameters # choose to either run all 1000 simulations or a subset (relevant if IBM is run on a cluster)

groups=c(1:10)# group IDs, correspond to Table S3.1


for(gg in 1:length(groups)){ # first loop: GROUPS
  
  g.name=groups[gg]

  # Pick initial conditinos for each group
  
  dataFirst=droplevels(lh[lh$ResidentGroup%in%g.name,])
  
  for(pu in 1:length(par.sub)){ # second loop: SIMULATIONS
    
    years=sample(c(1997:2014,2017,2018),30,replace=T)#randomly sample years
    years=rep(years,each=12)
    months=rep(c(1:12),30)
  
    print(paste("Iteration", pu))
    
    # Empty files to hold results for each simulation run in
    sim.data=NULL
    
    # Empty results to hold immigrants in
    tot.new.immig=0
    
    dataFirst$month=1
    dataFirst$year=as.character(years[1])
    dataFirst$rainSD=past.clim$rainSD[past.clim$year%in%years[1]&past.clim$month%in%1]
    dataFirst$tempSD=past.clim$tempSD[past.clim$year%in%years[1]&past.clim$month%in%1]
    
    data=dataFirst[,-1]
    
    data=data[,c("ID","Sex","AgeMonths","month","year","stage","pregCat","mass","group.size.sub","rainSD","tempSD","immig")]
    
    # Inital TB smtoms is always no
    tb="no"
    
    count=0 # to assign new IDs to new members of the group
    
    for(i in 2:length(years)){ # first date is alread given
      
      count=count+1
      
      ##### INITIAL STEPS #####
      
      # ASSIGN CLINICAL TB STATE TO GROUPS 
      groupTB=tb
      
      data$groupTB=groupTB
      
      ##### DEMOGRAPHIC RATES #####
      
      # SURVIVORS are drawn as binomial random variables
      
      data$surv=NA
      
      # pup survival
      if(length(data$surv[data$stage%in%"P"])>0) data$surv[data$stage%in%"P"]=rbinom(n = nrow(data[data$stage%in%"P",]), predict(pup.surv,newdata=data[data$stage%in%"P",],type="response"), size = 1)
      
      # juv survival
      if(length(data$surv[data$stage%in%"J"])>0) data$surv[data$stage%in%"J"]=rbinom(n = nrow(data[data$stage%in%"J",]), prob = predict(juv.surv,newdata=data[data$stage%in%"J",],type="response"), size = 1)
      
      # subadult survival
      if(length(data$surv[data$stage%in%"S"])>0) data$surv[data$stage%in%"S"]=rbinom(n = nrow(data[data$stage%in%"S",]), prob = predict(sub.surv,newdata=data[data$stage%in%"S",],type="response"), size = 1)
      
      # aldult helper survival (Male)
      if(length(data$surv[data$stage%in%"H"&data$Sex%in%"M"])>0) data$surv[data$stage%in%"H"&data$Sex%in%"M"]=rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"M",]), prob = predict(helpMale.surv,newdata=data[data$stage%in%"H"&data$Sex%in%"M",],type="response"), size = 1)
      
      # aldult helper survival (Female)
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F",])>0){
        if(any(data$pregCat[data$stage%in%"H"&data$Sex%in%"F"]%in%c("1"))){
          
          data$surv[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("1")]=1
        }
        
        if(any(data$pregCat[data$stage%in%"H"&data$Sex%in%"F"]%in%c("2"))){
          
          data$surv[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("2")]=rbinom(1,prob=0.985,size=1)
          
        }
        
        if(any(data$pregCat[data$stage%in%"H"&data$Sex%in%"F"]%in%c("birth"))){
          
          data$surv[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("birth")]=rbinom(1,prob=0.985,size=1)
        }
        
        if(any(data$pregCat[data$stage%in%"H"&data$Sex%in%"F"]%in%c("np"))){
          
          data$surv[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("np")]=rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("np"),]), prob = predict(helpFem.surv,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$pregCat%in%c("np"),],type="response"), size = 1)
          
          
        }
      }
      
      
      # dominant survival (Male, Female)
      
      # immigrant male
      if(length(data$surv[data$stage%in%"D"&data$Sex%in%"M"&data$immig%in%1])>0) data$surv[data$stage%in%"D"&data$Sex%in%"M"&data$immig%in%1]=rbinom(n = 1, prob = predict(domMale.surv,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&data$immig%in%1,],type="response"), size = 1)
      
      # natal male
      if(length(data$surv[data$stage%in%"D"&data$Sex%in%"M"&!data$immig%in%1])>0) data$surv[data$stage%in%"D"&data$Sex%in%"M"&!data$immig%in%1]=rbinom(n = 1, prob =predict(helpMale.surv,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&!data$immig%in%1,],type="response"), size = 1)
      
      # dominant female
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F",])>0){
        
        if(data$pregCat[data$stage%in%"D"&data$Sex%in%"F"]%in%c("1")){
          
          data$surv[data$stage%in%"D"&data$Sex%in%"F"]=1
        }else if(data$pregCat[data$stage%in%"D"&data$Sex%in%"F"]%in%c("birth")){
          
          data$surv[data$stage%in%"D"&data$Sex%in%"F"]=rbinom(n=1,prob=0.99,size=1)
        }else{
          
          data$surv[data$stage%in%"D"&data$Sex%in%"F"]=rbinom(n = 1, prob = predict(domFem.surv,newdata=data[data$stage%in%"D"&data$Sex%in%"F",],type="response"),size = 1)
          
        }  
      }
      
      # CONDITIONAL ON SURVIVAL
      
      # MASS CHANGE (drawn from Normal density distribution)
      
      data$massNext=NA
      
      # pups
      if(nrow(data[data$stage%in%"P"&data$surv%in%1,])>0) data$massNext[data$stage%in%"P"&data$surv%in%1]<-rnorm(n = nrow(data[data$stage%in%"P"&data$surv%in%1,]), mean = predict(pup.gr,data[data$stage%in%"P"&data$surv%in%1,]),
                                                                                                                 sd=sqrt(mean(residuals(pup.gr)^2)))
      
      # juveniles
      if(nrow(data[data$stage%in%"J"&data$surv%in%1,])>0) data$massNext[data$stage%in%"J"&data$surv%in%1]<-rnorm(n = nrow(data[data$stage%in%"J"&data$surv%in%1,]), mean = predict(juv.gr,data[data$stage%in%"J"&data$surv%in%1,]),
                                                                                                                 sd=sqrt(mean(residuals(juv.gr)^2)))
      # subadults
      if(nrow(data[data$stage%in%"S"&data$surv%in%1,])>0) data$massNext[data$stage%in%"S"&data$surv%in%1]<-rnorm(n = nrow(data[data$stage%in%"S"&data$surv%in%1,]), mean = predict(sub.gr,data[data$stage%in%"S"&data$surv%in%1,]),
                                                                                                                 sd=sqrt(mean(residuals(sub.gr)^2)))
      
      # Adult helper (Male Female)
      
      # MASS CHNAGE IS CONDITIONAL ON EMIGRATION 
      
      data$emig=NA
      
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1,])>0) data$emig[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1]<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1,]), prob = predict(helpMale.emig,newdata=data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1,],type="response"), size = 1)
      
      
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0,])>0) data$massNext[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0]<-rnorm(n = nrow(data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0,]), mean = predict(helpMale.gr,newdata=data[data$stage%in%"H"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0,],type="response"),
                                                                                                                                                                               sd=sqrt(mean(residuals(helpMale.gr)^2)))
      
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&!data$pregCat%in%"birth",])>0) data$emig[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&!data$pregCat%in%"birth"]<-rbinom(n =nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&!data$pregCat%in%"birth",]), prob = predict(helpFem.emig,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&!data$pregCat%in%"birth",],type="response"), size = 1)
      
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth",])>0) data$emig[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth"] = 0
      
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0,])>0) data$massNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0]<-rnorm(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0,]), mean = predict(helpFem.gr,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0,],type="response"),
                                                                                                                                                                               sd=sqrt(mean(residuals(helpFem.gr)^2)))
      
      # Adult dominant (Male Female)
      
      # Immigrant male
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$immig%in%1,])>0) data$emig[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$immig%in%1]<-rbinom(n = 1, prob = predict(domMale.emig,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$immig%in%1,],type="response"), size = 1)
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&data$immig%in%1,])>0) data$massNext[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&data$immig%in%1]<-rnorm(n = 1, mean = predict(domMale.gr,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&data$immig%in%1,],type="response"),
                                                                                                                                                                                                               sd=sqrt(mean(residuals(domMale.gr)^2)))
      
      
      
      
      # Natal male
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&!data$immig%in%1,])>0) data$emig[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&!data$immig%in%1]<-rbinom(n = 1, prob = predict(helpMale.emig,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&!data$immig%in%1,],type="response"), size = 1)
      
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&!data$immig%in%1,])>0) data$massNext[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&!data$immig%in%1]<-rnorm(n = 1, mean = predict(helpMale.gr,newdata=data[data$stage%in%"D"&data$Sex%in%"M"&data$surv%in%1&data$emig%in%0&!data$immig%in%1,],type="response"),
                                                                                                                                                                                                                 sd=sqrt(mean(residuals(helpMale.gr)^2)))
      
      
      
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1,])>0) data$massNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1]<-rnorm(n = 1, mean = predict(domFem.gr,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1,],type="response"),
                                                                                                                                                 sd=sqrt(mean(residuals(domFem.gr)^2)))
      # REPRODUCTION 
      
      ### CHANGE IN PREGNANCY CATEGORIES
      
      data$pregCatNext=NA
      
      data$pups=NA
      
      #helper 
      
      # NP to P1
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np",])>0){
        
        trans<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np",]), 
                      prob = predict(helpFem.NP_F_IM,newdata=data.frame(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np",2:5],immig.sum=sum(data$immig,na.rm = T),data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np",6:ncol(dataFirst)]),type="response"), size = 1)
        
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np"][trans==0] ="np"
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"np"][trans==1] ="1"
      } 
      
      # P1 to P2 
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1",])>0){
        
        trans<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1",]), 
                      prob = predict(helpFem.F_S,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1",],type="response"), size = 1)
        
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1"][trans==1] ="2"
        
        # P1 to P1
        if(any(trans==0)){
          
          trans2<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1",][trans==0,]), 
                         prob = predict(helpFem.F_F,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1",][trans==0,],type="response"), size = 1)
          
          data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1"][trans==0][trans2==0] ="np"
          data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"1"][trans==0][trans2==1] ="1"
          
        }
      }  
      
      # P2 to L (birth of litters) 
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2",])>0){
        
        trans<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2",]), 
                      prob = predict(helpFem.S_B,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2",],type="response"), size = 1)
        
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2"][trans==1] ="birth"
        
        # P2 to P1
        if(any(trans==0)){
          
          trans2<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2",][trans==0,]), 
                         prob = predict(helpFem.S_F,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2",][trans==0,],type="response"), size = 1)
          
          
          data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2"][trans==0][trans2==0] ="np"
          data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"2"][trans==0][trans2==1] ="1"
          
        }
      }  
      
      # L to pregnancy (P1) 
      if(nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",])>0){
        
        trans<-rbinom(n = nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",]), 
                      prob = predict(helpFem.B,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",],type="response"), size = 1)
        
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth"][trans==1] ="1"
        data$pregCatNext[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth"][trans==0] ="np"
        
        # RECRUTIMENT
        
        data$pups[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth"] = rpois(n=nrow(data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",]),
                                                                                                                   predict(helpFem.pups,newdata=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",],type="response"))
        
        
        if(any(data$pups[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth"])>0){
          
          df=data[data$stage%in%"H"&data$Sex%in%"F"&data$surv%in%1&data$emig%in%0&data$pregCat%in%"birth",]
          pups.data=df[rep(row.names(df), df$pups),]
          
          pups.data$groupTB=groupTB
          
          pups.data$mom.mass=pups.data$mass
          
          pups.data$mass=NA
          
          # Pup mass
          pups.data$mass <-rnorm(n = nrow(pups.data), mean = predict(pupsH.mass,pups.data),sd=sqrt(mean(residuals(pupsH.mass)^2)))
          
          pups.data$mass[pups.data$mass>max.mass.pup&!is.na(pups.data$mass)]=max.mass.pup
          
          # Pup sex
          pups.data$Sex=NA
          pups.data$Sex=sample(c("M","F"),nrow(pups.data),prob=c(0.5,0.5),replace = T)
          
          pups.data=pups.data[,-grep("groupTB",colnames(pups.data))]
        } 
        
      }  
      
      #dominant 
      
      # NP to P1
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"np",])>0){
        
        trans<-rbinom(n = 1, 
                      prob = predict(domFem.NP_F_IM,newdata=data.frame(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"np",2:5],immig.sum=sum(data$immig,na.rm = T),data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"np",6:ncol(dataFirst)]),type="response"), size = 1)
        
        if(trans==0) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"np"] <-"np" else data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"np"] <-"1"
        
      } 
      
      # P1 to P2 
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1",])>0){
        
        trans<-rbinom(n = 1, 
                      prob = predict(domFem.F_S,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1",],type="response"), size = 1)
        
        if(trans==1) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1"] <- "2"
        
        if(trans==0){
          
          trans2<-rbinom(n = 1, 
                         prob = tryCatch(predict(domFem.F_F,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1",][trans==0,],type="response"),
                                         error=function(e) predict(domFem.F_F,newdata=data.frame(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1",2:4],year=2000,data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1",6:ncol(dataFirst)])[trans==0,],type="response",exclude = "s(year)")), size = 1)
          
          if(trans2==0) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1"] <- "np"
          
          if(trans2==1) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"1"] <- "1"
          
        }
      }  
      
      # P2 to L (birth to litters) 
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2",])>0){
        
        trans<-rbinom(n = 1, 
                      prob = predict(domFem.S_B,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2",],type="response"), size = 1)
        
        if(trans==1) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2"] <- "birth"
        
        if(trans==0){
          
          # P2 to P1
          trans2<-rbinom(n = 1, 
                         prob = predict(domFem.S_F,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2",],type="response"), size = 1)
          
          if(trans2==1) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2"] <- "1" else if(trans2==0) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"2"] <- "np" 
          
        }
      }  
      
      # L (Birth) to P1 
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth",])>0){
        
        trans<-rbinom(n = 1, 
                      prob =predict(domFem.B_NP,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth",],type="response"), size = 1)
        
        if(trans==0) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth"] <- "np"
        
        if(trans==1) data$pregCatNext[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth"] <- "1"
        
        
        # RECRUTIMENT
        
        data$pups[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth"] = rpois(n=1,predict(domFem.pups,newdata=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth",],type="response"))
        
        
        if(data$pups[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth"]>0){
          
          df=data[data$stage%in%"D"&data$Sex%in%"F"&data$surv%in%1&data$pregCat%in%"birth",]
          pups.dataDom=df[rep(row.names(df), df$pups),]
          
          pups.dataDom$mom.mass=pups.dataDom$mass
          
          pups.dataDom$mass=NA
          
          pups.dataDom$groupTB=groupTB
          
          # Pup mass
          pups.dataDom$mass <-rnorm(n = nrow(pups.dataDom), mean = predict(pupsD.mass,pups.dataDom),sd=sqrt(mean(residuals(pupsD.mass)^2)))
          
          pups.dataDom$mass[pups.dataDom$mass>max.mass.pup&!is.na(pups.dataDom$mass)]=max.mass.pup
          
          # Pup sex
          pups.dataDom$Sex=NA
          pups.dataDom$Sex=sample(c("M","F"),nrow(pups.dataDom),prob=c(0.5,0.5),replace = T)
          
          pups.dataDom=pups.dataDom[,-grep("groupTB",colnames(pups.dataDom))]
          
        }
        
      }  
      
      
      
      #update data
      if(length(data$massNext[data$massNext>max.mass.adult&!is.na(data$massNext)])>0) data$massNext[data$massNext>max.mass.adult&!is.na(data$massNext)] <- max.mass.adult
      
      # save old
      time.sim=i-1
      sim.data=rbind(sim.data,cbind(data,time.sim))
      
      ##### CHANGE CLINICAL TB STATUS #####
      
      if(groupTB=="yes"){tb<-"yes"}else if(i<6){tb<-"no"}else{
        
        
        probsymp=rbinom(n=1,prob=predict(TBsymp,newdata = data.frame(month=months[i],im=sum(tot.new.immig[(i-5):(i-1)]),temp=past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1]),
                                         type="response"),size=1)
        
        if(probsymp==1) tb<- "yes" else tb<- "no"
        
        
      }
      
      print(tb)
      
      
      ##### NEW  DATA FOR T + 1 #####
      
      data=data[data$surv%in%1&data$emig%in%c(0,NA),]
      
      
      if(nrow(data)<1) break
      
      data$AgeMonths=data$AgeMonths+1
      data$month=months[i]
      data$year=as.character(years[i])
      data$stage=as.character(data$stage)
      data$stage[!data$stage%in%c("H","D")&data$AgeMonths%in%(1:3)]="P"
      data$stage[!data$stage%in%c("H","D")&data$AgeMonths%in%(4:6)]="J"
      data$stage[!data$stage%in%c("H","D")&data$AgeMonths%in%(7:12)]="S"
      data$stage[!data$stage%in%c("H","D")&data$AgeMonths>12]="H"
      data$pregCat=data$pregCatNext
      data$pregCat[data$Sex%in%"F"&data$stage%in%"H"&is.na(data$pregCat)]="np"
      data$mass=data$massNext
      data$rainSD=past.clim$rainSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1]
      data$tempSD=past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1]
      
      
      
      data$immig[data$immig%in%1]=1
      
      data=data[,c("ID","Sex","AgeMonths","month","year","stage","pregCat","mass","rainSD","tempSD","immig")]
      
      ##### UPDATE DOMINANCE #####
      
      # If dominant female dies, oldest and heaviest helper becomes dominant
      
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"F",])<1){
        
        H_to_D=data[data$stage%in%"H"&data$Sex%in%"F",][order(data[data$stage%in%"H"&data$Sex%in%"F","AgeMonths"],data[data$stage%in%"H"&data$Sex%in%"F","mass"],decreasing = T),][1,"ID"]
        
        data[data$ID%in%H_to_D,"stage"] = "D"
        
      }
      
      # If dominant male dies or emigrates, oldest and heaviest immigrant helper becomes dominant. If no immigrant available, natal male takes over. If no adult helpers available to take over, new immigrant takes over
      
      new.immig=0
      
      if(nrow(data[data$stage%in%"D"&data$Sex%in%"M",])<1){
        
        # if there are any old immigrant 
        if(nrow(data[data$stage%in%"H"&data$Sex%in%"M",])>0&any(data$immig%in%1)){
          
          H_to_D=data[data$immig%in%1,][order(data[data$immig%in%1,"AgeMonths"],data[data$immig%in%1,"mass"],decreasing = T),][1,"ID"]
          
          data[data$ID%in%H_to_D,"stage"][1] = "D"
          
          # if not, natal male 
        }else if(nrow(data[data$stage%in%"H"&data$Sex%in%"M",])>0&!any(data$immig%in%1)){
          
          H_to_D=data[data$stage%in%"H"&data$Sex%in%"M",][order(data[data$stage%in%"H"&data$Sex%in%"M","AgeMonths"],data[data$stage%in%"H"&data$Sex%in%"M","mass"],decreasing = T),][1,"ID"]
          
          data[data$ID%in%H_to_D,"stage"][1] = "D"
          
        }else{ # new immigrant
          
          data=rbind(data,data.frame(ID=paste(count,years[i],months[i],nrow(data)+1),Sex="M",
                                     AgeMonths=round(age.mass.domM$AgeMonths[age.mass.domM$year%in%years[i]&age.mass.domM$month%in%months[i]]),
                                     month=months[i],
                                     year=as.character(years[i]),
                                     stage="D",
                                     pregCat=NA,
                                     mass=mean(age.mass.domM$mass[age.mass.domM$month%in%months[i]],na.rm=T),
                                     rainSD=past.clim$rainSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],
                                     tempSD=past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],immig=1))
          new.immig=1
        } 
        
        
        
      }
      
      
      ## Add pups
      
      if(exists("pups.data")){
        
        data=rbind(data,data.frame(ID=paste(count,years[i],months[i],"momH","momID:",pups.data$ID,seq(nrow(data)+1,nrow(data)+1+nrow(pups.data)-1)),
                                   Sex=pups.data$Sex,
                                   AgeMonths=1,
                                   month=months[i],
                                   year=as.character(years[i]),
                                   stage="P",
                                   pregCat=NA,
                                   mass=pups.data$mass,
                                   rainSD=past.clim$rainSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],
                                   tempSD=past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],immig=0))
        
        rm(pups.data)
      }
      
      if(exists("pups.dataDom")){
        
        data=rbind(data,data.frame(ID=paste(count,months[i],years[i],"momD","momID:",pups.dataDom$ID,seq(nrow(data)+1,nrow(data)+1+nrow(pups.dataDom)-1)),
                                   Sex=pups.dataDom$Sex,
                                   AgeMonths=1,
                                   month=months[i],
                                   year=as.character(years[i]),
                                   stage="P",
                                   pregCat=NA,
                                   mass=pups.dataDom$mass,
                                   rainSD=past.clim$rainSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],
                                   tempSD=past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],immig=0))
        
        rm(pups.dataDom)
      }
      
      data.immig=data[1,]
      data.immig$group.size.sub=nrow(data[data$stage%in%c("S","H","D"),])-new.immig
      data.immig$n.male=nrow(data[data$Sex%in%"M"&data$stage%in%c("S","H","D"),])-new.immig
      data.immig$n.fem=nrow(data[data$Sex%in%"F"&data$stage%in%c("S","H","D"),])
      data.immig$r_f_m=data.immig$n.fem/data.immig$n.male
      data.immig$groupTB=groupTB
      data.immig$r_f_m[!is.finite(data.immig$r_f_m)]= data.immig$n.fem
     
      
      ##### NEW HELPER IMMIGRANTS #####
      
      new.immigH=0
      
      
      immigHProb=rbinom(n = 1, 
                        prob = predict(probImmig,newdata=data.immig,type="response"), size = 1)
      
      if(immigHProb%in%1) immigH<-max(1,rpois(1,predict(numImmig,newdata=data.immig,type="response")))
      
      
      if(exists("immigH")){
        
        new.immigH=immigH
        
        print("immig")
        
        if(immigH>0){
          
          data=rbind(data,data.frame(ID=paste(count,years[i],months[i],seq(1,immigH)),
                                     Sex=rep("M",immigH),
                                     AgeMonths=rpois(immigH,43),
                                     month=rep(months[i],immigH),
                                     year=as.character(rep(years[i],immigH)),
                                     stage=rep("H",immigH),
                                     pregCat=rep(NA,immigH),
                                     mass=rnorm(immigH,mean=6.56,sd=0.09),
                                     rainSD=rep(past.clim$rainSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],immigH),
                                     tempSD=rep(past.clim$tempSD[past.clim$year%in%as.character(years[i])&past.clim$month%in%months[i]][1],immigH),
                                     immig=rep(1,immigH)))
          
        }
        
        rm(immigH) 
      }
      
      tot.new.immig=c(tot.new.immig,new.immig+new.immigH)
      
      data$group.size.sub=nrow(data[data$stage%in%c("S","H","D"),])
      
      data$Sex=as.character(data$Sex)
      data$year=as.character(data$year)
      
      if(nrow(data[data$AgeMonths>6,])<1) break
      
      if(nrow(data[data$stage%in%c("S","H","D"),])<2) break
      
      if(all(unique(data[data$AgeMonths>6,"Sex"])%in%"M")) break
      
      
    }
    
    sim.data$run=par.sub[pu]
    sim.data$group=groups[gg]
    
    ibm.data=rbind(ibm.data,sim.data)
    
  }
}



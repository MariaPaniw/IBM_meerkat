rm(list=ls(all=TRUE))

library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(ggplot2)
library(lubridate)
library(dplyr)

library(rgeos)
library(sf)
library(FNN)

# Study site location

xy.kmp=data.frame(lon=21.83417,lat=-26.98)

xy.kmp.sp<-st_as_sf(xy.kmp,coords=c("lon","lat"))

### !!! NOTE !!!
# Please download all climate files from 10.6084/m9.figshare.16794001

### !!! NOTE !!!

# It is not necessary to run the entire script. To replicate plots in S3.2, just upload the .csv files from GitHub as indicated at the bottom of this script

setwd("...")

files=list.files("...") # list all climate files in your directory 

dname <- "tasmax"

gcm=c("MPI-ESM-MR","IPSL-CM5A-MR","MIROC5","GISS-E2-H","inmcm4","FIO-ESM","NorESM1","CSIRO-Mk3-6-0","ACCESS1-0",
      "CanESM2","bcc-csm1-1-m","IPSL-CM5A-LR","CNRM-CM5","GFDL-ESM2G","FGOALS-g2","MRI-CGCM3","MIROC-ESM-CHEM","GISS-E2-R",
      "CCSM4","BNU-ESM","EC-EARTH")

scen=c("rcp26","rcp45","rcp60","rcp85")

scen.nice=c("RCP2.6","RCP4.5","RCP6.0","RCP8.5")

cutoff=c(6:10) # monthly cutoff of above-average temperatures

grid.point=1 # set to 1 or 2: decide if you want to interpolate at closest grid points (1) or 2 closest grid points (2)

# Empty data frames to hold results in
dat=NULL
proj.sd=NULL
extremes=NULL

for(i in 1:length(gcm)){
  
  #Get historical 
  
  # open a NetCDF file
  sub.file=files[grep(gcm[i],files)]
  sub.file=sub.file[grep("historical",sub.file)]
  
  temporHist=NULL
  
  for(y in 1:length(sub.file)){
    
    ncin <- nc_open(sub.file[y])
    
    print(ncin)
    
    lon <- ncvar_get(ncin, "lon")
    nlon <- dim(lon)
    head(lon)
    
    lon=ifelse(lon > 180, -360 + lon, lon)
    
    lat <- ncvar_get(ncin, "lat", verbose = F)
    nlat <- dim(lat)
    head(lat)
    
    print(c(nlon,nlat))
    
    t <- ncvar_get(ncin, "time")
    tunits <- ncatt_get(ncin, "time", "units")
    nt <- dim(t)
    
    tmp.array <- ncvar_get(ncin, dname)
    dlname <- ncatt_get(ncin, dname, "long_name")
    dunits <- ncatt_get(ncin, dname, "units")
    fillvalue <- ncatt_get(ncin, dname, "missing_value")
    dim(tmp.array)
    
    title <- ncatt_get(ncin, 0, "title")
    institution <- ncatt_get(ncin, 0, "institution")
    datasource <- ncatt_get(ncin, 0, "source")
    references <- ncatt_get(ncin, 0, "references")
    history <- ncatt_get(ncin, 0, "history")
    Conventions <- ncatt_get(ncin, 0, "Conventions")
    
    nc_close(ncin)
    
    # split the time units string into fields
    tustr <- strsplit(tunits$value, " ")
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth = as.integer(unlist(tdstr)[2])
    tday = as.integer(unlist(tdstr)[3])
    tyear = as.integer(unlist(tdstr)[1])
    
    
    tmp.array[tmp.array == fillvalue$value] <- NA
    length(na.omit(as.vector(tmp.array[, , 1])))
    
    m <- 1
    
    # Get closest point (option 1) or closest 2 points (option 2) of interpolation to study site:
    
    
    xy.clim=expand.grid(lon,lat)
    
    xy.clim.sp<-st_as_sf(xy.clim,coords=c("Var1","Var2"))
    
    if(grid.point==1){
      g<-get.knnx(as.matrix(st_coordinates(xy.clim.sp)),as.matrix(st_coordinates(xy.kmp.sp)),k=1)
      
      lon.sub=which(lon%in%xy.clim[g$nn.index,][,1])
      lat.sub=which(lat%in%xy.clim[g$nn.index,][,2])
      sub=tmp.array[lon.sub,lat.sub, ]
      
    }else if(grid.point==2){
      
      g<-get.knnx(as.matrix(st_coordinates(xy.clim.sp)),as.matrix(st_coordinates(xy.kmp.sp)),k=2)
      
      lon.sub=which(lon%in%xy.clim[g$nn.index,][,1])
      lat.sub=which(lat%in%xy.clim[g$nn.index,][,2])
      sub=apply(tmp.array[lon.sub,lat.sub, ],length(dim(tmp.array[lon.sub,lat.sub, ])),mean)
      
    }
    
    temporHistXXX=data.frame(lon=xy.clim[g$nn.index,][1,1],lat=xy.clim[g$nn.index,][1,2],temp=sub)
    
    temporHistXXX$date=chron(t, origin = c(tmonth, tday, tyear))
    
    temporHistXXX$rcp="historical"
    temporHistXXX$gcm=gcm[i]
    temporHistXXX$temp=temporHistXXX$temp- 273.15
    temporHistXXX$date=as.Date(temporHistXXX$date)
    
    temporHist=rbind(temporHist,temporHistXXX)
  }
  
  dat=rbind(dat,temporHist[temporHist$date>"1996-12-31"&temporHist$date<"2006-01-01",]) # hsitorical time series for plotting
  
  temporHist$month=month(temporHist$date)
  
  # Loop through RCP scenarios
  for(j in 1:length(scen)){
    
    # open a NetCDF file
    sub.file=files[grep(gcm[i],files)]
    sub.file=sub.file[grep(scen[j],sub.file)]
    
    if(length(sub.file)>0){
      
      temporALL=NULL
      for(x in 1:length(sub.file)){
        
        ncin <- nc_open(sub.file[x])
        
        print(ncin)
        
        lon <- ncvar_get(ncin, "lon")
        nlon <- dim(lon)
        head(lon)
        
        lon=ifelse(lon > 180, -360 + lon, lon)
        
        lat <- ncvar_get(ncin, "lat", verbose = F)
        nlat <- dim(lat)
        head(lat)
        
        print(c(nlon,nlat))
        
        t <- ncvar_get(ncin, "time")
        tunits <- ncatt_get(ncin, "time", "units")
        nt <- dim(t)
        
        tmp.array <- ncvar_get(ncin, dname)
        dlname <- ncatt_get(ncin, dname, "long_name")
        dunits <- ncatt_get(ncin, dname, "units")
        fillvalue <- ncatt_get(ncin, dname, "missing_value")
        dim(tmp.array)
        
        title <- ncatt_get(ncin, 0, "title")
        institution <- ncatt_get(ncin, 0, "institution")
        datasource <- ncatt_get(ncin, 0, "source")
        references <- ncatt_get(ncin, 0, "references")
        history <- ncatt_get(ncin, 0, "history")
        Conventions <- ncatt_get(ncin, 0, "Conventions")
        
        nc_close(ncin)
        
        # split the time units string into fields
        tustr <- strsplit(tunits$value, " ")
        tdstr <- strsplit(unlist(tustr)[3], "-")
        tmonth = as.integer(unlist(tdstr)[2])
        tday = as.integer(unlist(tdstr)[3])
        tyear = as.integer(unlist(tdstr)[1])
        
        tmp.array[tmp.array == fillvalue$value] <- NA
        length(na.omit(as.vector(tmp.array[, , 1])))
        
        m <- 1
        
        # Get closest point of interpolation to study site:
        
        xy.clim=expand.grid(lon,lat)
        
        xy.clim.sp<-st_as_sf(xy.clim,coords=c("Var1","Var2"))
        
        if(grid.point==1){
          g<-get.knnx(as.matrix(st_coordinates(xy.clim.sp)),as.matrix(st_coordinates(xy.kmp.sp)),k=1)
          
          lon.sub=which(lon%in%xy.clim[g$nn.index,][,1])
          lat.sub=which(lat%in%xy.clim[g$nn.index,][,2])
          sub=tmp.array[lon.sub,lat.sub, ]
          
        }else if(grid.point==2){
          
          g<-get.knnx(as.matrix(st_coordinates(xy.clim.sp)),as.matrix(st_coordinates(xy.kmp.sp)),k=2)
          
          lon.sub=which(lon%in%xy.clim[g$nn.index,][,1])
          lat.sub=which(lat%in%xy.clim[g$nn.index,][,2])
          sub=apply(tmp.array[lon.sub,lat.sub, ],length(dim(tmp.array[lon.sub,lat.sub, ])),mean)
          
        }
        
        
        tempor=data.frame(lon=xy.clim[g$nn.index,][1,1],lat=xy.clim[g$nn.index,][1,2],temp=sub)
        
        tempor$date=chron(t, origin = c(tmonth, tday, tyear))
        
        tempor$rcp=scen.nice[j]
        tempor$gcm=gcm[i]
        tempor$temp=tempor$temp- 273.15
        tempor$date=as.Date(tempor$date)
        
        dat=rbind(dat,tempor[tempor$date<"2101-01-01",])
        
        temporALL=rbind(temporALL,tempor)
      }
      
      #### STANDARDIZED DEVIATION 
      
      temporALL$month=month(temporALL$date)
      
      temporALL=temporALL[temporALL$date<"2101-01-01",]
      
      # get historic mean of monthly lagged value
      hist=rbind(temporHist[temporHist$date>"1996-12-31"&temporHist$date<"2006-01-01",],temporALL)
      hist$tempMAXlag=NA
      
      for(k in 2:nrow(hist)){
        
        hist$tempMAXlag[k]=mean(hist$temp[k-1], hist$temp[k])
        
      }
      
      ### !!! Note, you do not have to scale !!! If you do not wish to do so, just comment out the line below 
      hist$tempMAXlag=as.numeric(scale(hist$tempMAXlag))
      
      # Create new average temperature: 
      mean.hist=aggregate(tempMAXlag~month,data=hist[hist$date<"2019-01-15",],mean,na.rm=T)
      
      colnames(mean.hist)[2]="temp.mu"
      
      # join average monthly values to main data frame
      hist$tempMean=left_join(hist,mean.hist,by="month")$temp.mu
      hist$tempSD=hist$tempMAXlag-hist$tempMean
      
      ### How many extreme years
      
      
      proj.sd=rbind(proj.sd,hist[-which(is.na(hist$tempSD)),])
      
      histNoNA=hist[hist$date<"2019-01-15"&!is.na(hist$tempMAXlag),]
      histNoNA$year=year(histNoNA$date)
      
      ## Mid Century
      
      projMidC=hist[hist$date>"2040-12-14"&hist$date<"2062-01-01",]
      projMidC$year=year(projMidC$date)
      
      ## End Century
      projEndC=hist[hist$date>"2078-12-14"&hist$date<"2100-01-01",]
      projEndC$year=year(projEndC$date)
      
      ### How many extreme years
      for(cc in 1:length(cutoff)){
        
        ext.histNoNA=length(table(histNoNA$year[histNoNA$tempSD>0])[table(histNoNA$year[histNoNA$tempSD>0])>cutoff[cc]])/length(unique(histNoNA$year))
        ext.projMidC=length(table(projMidC$year[projMidC$tempSD>0])[table(projMidC$year[projMidC$tempSD>0])>cutoff[cc]])/length(unique(projMidC$year))
        ext.projEndC=length(table(projEndC$year[projEndC$tempSD>0])[table(projEndC$year[projEndC$tempSD>0])>cutoff[cc]])/length(unique(projEndC$year))
        
        extremes=rbind(extremes,data.frame(hist=rep(ext.histNoNA,2),proj=c(ext.projMidC,ext.projEndC),run=c("midC","endC"),rcp=scen.nice[j],gcm=gcm[i],cutoff=cutoff[cc]))
        
      }
      
    
    }
    
  }
  
}

library(scales)


a=ggplot(data=proj.sd,aes(date,tempSD,col=rcp))+
  geom_line()+
  facet_wrap(gcm~.,ncol=3)+
  theme_bw(base_size = 24)+
  xlab("Time") +
  # guides(col=F)+
  scale_color_viridis_d(name="",option="B",begin=0,end=0.9)+
  ylab("Deviation from monthly mean (1997-2018)")+
  # theme(panel.grid = element_blank())+
  theme(axis.text.y = element_text(color="black"))+
  theme(axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=0.9),
        plot.title = element_text(size=22),
        legend.position = "top",
        legend.key.size = unit(5,"line"))+
  geom_vline(xintercept = as.Date("2019-01-01"),col="red",size=1.5)+
  theme(plot.margin = unit(c(0.2,0.1,0.5,0.1), "cm"))+
  scale_x_date(date_breaks = "180 month",date_minor_breaks = "24 month", labels = date_format("%Y"), expand = c(0, 0))+
  theme(axis.text.x = element_text(color="black"))


b=ggplot(data=dat,aes(date,temp,col=rcp))+
  geom_line()+
  facet_wrap(gcm~.,ncol=3)+
  theme_bw(base_size = 24)+
  xlab("Time") +
  # guides(col=F)+
  scale_color_viridis_d(name="",option="B",begin=0,end=0.9)+
  ylab("Mean monthly maximum temperatures (ºC)")+
  # theme(panel.grid = element_blank())+
  theme(axis.text.y = element_text(color="black"))+
  theme(axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=0.9),
        plot.title = element_text(size=22),
        legend.position = "top",
        legend.key.size = unit(5,"line"))+
  geom_vline(xintercept = as.Date("2019-01-01"),col="red",size=1.5)+
  theme(plot.margin = unit(c(0.2,0.1,0.5,0.1), "cm"))+
  scale_x_date(date_breaks = "180 month",date_minor_breaks = "24 month", labels = date_format("%Y"), expand = c(0, 0))+
  theme(axis.text.x = element_text(color="black"))


extremes$run=factor(extremes$run,levels=c("midC","endC"))
levels(extremes$run)=c("Mid century","End century")

extremes$cutoff=factor(extremes$cutoff)

levels(extremes$cutoff)=c(">6 m",">7 m",">8 m",">9 m",">10 m")

### Alternatively, load this csv 

extremes= read.csv("gcm_output.csv") # for data interpolated at closest grid point 

# extremes=read.csv("/Users/maria/Dropbox/collaborations/KMP_IBM_ERC/output/gcm_output_2grid_point_interpol.csv") # for data interpolated at 2 closest grid points

c=ggplot(data=extremes,aes(hist))+
  geom_density(fill="grey80",col=NA,alpha=0.5)+
  geom_density(aes(proj,fill=rcp),col=NA,alpha=0.5)+
  scale_fill_viridis_d(name="",option="C",begin=0.3,end=0.9)+
  facet_grid(run~cutoff,scales="free_y")+
  # geom_histogram(aes(proj),fill="red",col="black",alpha=0.4)+
  theme_bw(base_size = 20)+
  xlab("Proportion extreme years") +
  guides(col=F)+ 
  ylab("Count")+
  scale_x_continuous(breaks=c(0.0,0.25,0.5,0.75,1.0),labels=c("0.0","0.25","0.5","0.75","1.0"))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(c(0.2,0.6,0.2,0.2), "cm"))+
  theme(axis.text = element_text(color="black"),
        legend.position = "top")

c

# Proportions of GCMs where hot years double in occurrence (considering only outputs where hot years currently occur with >= 0.1 probability)

extremes$double="no"

extremes$double[extremes$proj>=(extremes$hist*2)]="yes"

extremes$double[extremes$proj==0|extremes$proj<0.1]="no"

risk=aggregate(double~rcp+run+cutoff,data=extremes,function(x) length(x[x%in%"yes"])/length(x))


## Proportion of GCMs where hot years occur with a probability of 0.75 or more  
riskHigh=aggregate(proj~rcp+run+cutoff,data=extremes,function(x) length(x[x>0.75])/length(x))



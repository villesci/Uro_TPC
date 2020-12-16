library(dplyr)
library(ggplot2)
library(forcats)
library(PerformanceAnalytics)
library(sjPlot)
library(lubridate)
library(HH)
library(ggiraphExtra)
library(extrafont)
library(here)
library(lubridate)
library(plyr)
library(tidyr)
library(digest)
library(ggnewscale)
library(akima)
library(padr)
library(metR)
library(scales)
loadfonts(device = "win")

##temperature data



gbj<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/gbj.csv"),header=T)
gbj$rdate<-as.POSIXct(gbj$DateTimeStamp,tz="","%m/%d/%Y%H:%M")
gbj$oce<-"a"
wh<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/wh.csv"),header=T)
wh$rdate<-as.POSIXct(wh$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
wh$oce<-"a"
oy<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/oy.csv"),header=T)
oy$rdate<-as.POSIXct(oy$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
oy$oce<-"a"
bf<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/bf.csv"),header=T)
bf$rdate<-as.POSIXct(bf$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
bf$oce<-"a"
fb<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/fb.csv"),header=T)
fb$rdate<-as.POSIXct(fb$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
fb$oce<-"a"
gcsk<-read.csv(here::here("data/environmental_data/rawAtlantic/atl/gcsk.csv"),header=T)
gcsk$rdate<-as.POSIXct(gcsk$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
gcsk$oce<-"a"

nah1516<-read.csv(here::here("data/environmental_data/rawPacific/pac/nahcotta_2015_2016.csv"),header=T)#2015 through August, 2016 data after that
nah1516$rdate<-as.POSIXct(nah1516$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")
nah1516$oce<-"p"
hmi2<-read.csv(here::here("data/environmental_data/rawPacific/pac/hmi2.csv"),header=T)
hmi2$rdate<-as.POSIXct(hmi2$DateTimeStamp,tz="", "%m/%d/%Y%H:%M")#Indian island 2015
hmi2$oce<-"p"
to3<-read.csv(here::here("data/environmental_data/rawPacific/pac/to3.csv"),header=T)
to3$rdate<-as.POSIXct(to3$DateTimeStamp,tz="","%m/%d/%Y%H:%M")##stitched 2014 and 2015 (post Nove. 21 data together)
to3$oce<-"p"

#create objects of tempreatures during summer only 
s.gbj<-filter(gbj,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.wh<-filter(wh,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.oy<-filter(oy,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.bf<-filter(bf,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.fb<-filter(fb,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.to3<-filter(to3,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.hmi2<-filter(hmi2,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.nah1516<-filter(nah1516,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.gcsk<-filter(gcsk,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")

##Quartiles
q<-data.frame("site" = c("gbj","wh","oy","bf","fb","nah1516","hmi2","to3","gcsk"),"quantile"=NA,"decile"=NA, "max"=NA, "mean"=NA,"summer mean"=NA,"seasonlength10"=NA,"seasonlength12"=NA)
q[1,2]<-quantile(s.gbj$WTMP,0.75,type=1)
q[2,2]<-quantile(s.wh$WTMP,0.75,type=1)
q[3,2]<-quantile(s.oy$WTMP,0.75,type=1)
q[4,2]<-quantile(s.bf$WTMP,0.75,type=1)
q[5,2]<-quantile(s.fb$WTMP,0.75,type=1)
q[6,2]<-quantile(s.nah1516$WTMP,0.75,type=1)
q[7,2]<-quantile(s.hmi2$WTMP,0.75,type=1)
q[8,2]<-quantile(s.to3$WTMP,0.75,type=1)
q[9,2]<-quantile(s.gcsk$WTMP,0.75,type=1)

#upper 90th
q[1,3]<-quantile(s.gbj$WTMP,0.9,type=1)
q[2,3]<-quantile(s.wh$WTMP,0.9,type=1)
q[3,3]<-quantile(s.oy$WTMP,0.9,type=1)
q[4,3]<-quantile(s.bf$WTMP,0.9,type=1)
q[5,3]<-quantile(s.fb$WTMP,0.9,type=1)
q[6,3]<-quantile(s.nah1516$WTMP,0.9,type=1)
q[7,3]<-quantile(s.hmi2$WTMP,0.9,type=1)
q[8,3]<-quantile(s.to3$WTMP,0.9,type=1)
q[9,3]<-quantile(s.gcsk$WTMP,0.9,type=1)

#maximum temperature
q[1,4]<-s.gbj %>%  summarise(Value = max(WTMP))
q[2,4]<-s.wh  %>% summarise(Value = max(WTMP))
q[3,4]<-s.oy  %>% summarise(Value = max(WTMP))
q[4,4]<-s.bf  %>% summarise(Value = max(WTMP))
q[5,4]<-s.fb  %>% summarise(Value = max(WTMP))
q[6,4]<-s.nah1516 %>% summarise(Value = max(WTMP))
q[7,4]<-s.hmi2  %>% summarise(Value = max(WTMP))
q[8,4]<-s.to3  %>% summarise(Value = max(WTMP))
q[9,4]<-s.gcsk  %>% summarise(Value=max(WTMP))

#means
q[1,5]<-mean(gbj$WTMP)
q[2,5]<-mean(wh$WTMP)
q[3,5]<-mean(oy$WTMP)
q[4,5]<-mean(bf$WTMP)
q[5,5]<-mean(fb$WTMP)
q[6,5]<-mean(nah1516$WTMP)
q[7,5]<-mean(hmi2$WTMP)
q[8,5]<-mean(to3$WTMP)
q[9,5]<-mean(gcsk$WTMP)

#summer means
#means
q[1,6]<-mean(s.gbj$WTMP)
q[2,6]<-mean(s.wh$WTMP)
q[3,6]<-mean(s.oy$WTMP)
q[4,6]<-mean(s.bf$WTMP)
q[5,6]<-mean(s.fb$WTMP)
q[6,6]<-mean(s.nah1516$WTMP)
q[7,6]<-mean(s.hmi2$WTMP)
q[8,6]<-mean(s.to3$WTMP)
q[9,6]<-mean(s.gcsk$WTMP)

#season length

q[1,8]<-gbj%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[2,8]<-wh%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[3,8]<-oy%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[4,8]<-bf%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[5,8]<-fb%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[6,8]<-nah1516%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[7,8]<-hmi2%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[8,8]<-to3%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally
q[9,8]<-gcsk%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>12.5)%>%tally

q[1,7]<-gbj%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[2,7]<-wh%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[3,7]<-oy%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[4,7]<-bf%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[5,7]<-fb%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[6,7]<-nah1516%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[7,7]<-hmi2%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[8,7]<-to3%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally
q[9,7]<-gcsk%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)%>%tally


pac2<-rbind(hmi2,nah1516,to3)
atll<-rbind(gcsk,fb,bf,oy,wh,gbj)

temp<-rbind(pac2,atll) #temperature data for all

means<-data.frame(with(temp,tapply(WTMP,site,mean)))

#summer means

summer.temp<-filter(temp,rdate>"2018-06-01 00:00:00" & rdate< "2018-09-30 00:00:00")
s.mean<-data.frame(with(summer.temp,tapply(WTMP,site,mean)))

temp$date<-as.Date(temp$rdate)

temp$site<-factor(temp$site, levels=c("gbj","wh","oy","bf","fb","GCSK","nah15","hmi2","to3"))
#"Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"

#egg lay dates
#egg laying mean==
lay_sk<-mean((filter(gcsk,rdate>"2018-03-01 00:00:00" & rdate< "2018-03-30 00:00:00"))$WTMP)
lay_fb<-mean((filter(fb,rdate>"2018-03-01 00:00:00" & rdate< "2018-03-30 00:00:00"))$WTMP)
lay_bf<-mean((filter(bf,rdate>"2018-03-15 00:00:00" & rdate< "2018-04-15 00:00:00"))$WTMP)
lay_oy<-mean((filter(oy,rdate>"2018-05-01 00:00:00" & rdate< "2018-05-30 00:00:00"))$WTMP)
lay_wh<-mean((filter(wh,rdate>"2018-05-15 00:00:00" & rdate< "2018-06-15 00:00:00"))$WTMP)
lay_gb<-mean((filter(gbj,rdate>"2018-06-01 00:00:00" & rdate< "2018-06-30 00:00:00"))$WTMP)
lay_hm<-mean((filter(hmi2,rdate>"2018-04-15 00:00:00" & rdate< "2018-05-15 00:00:00"))$WTMP)#inat
lay_wp<-mean((filter(nah1516,rdate>"2018-04-15 00:00:00" & rdate< "2018-05-15 00:00:00"))$WTMP)#ruesink
#max
lay_sk2<-mean((filter(gcsk,rdate>"2018-03-01 00:00:00" & rdate< "2018-05-30 00:00:00"))$WTMP)
lay_fb2<-mean((filter(fb,rdate>"2018-03-01 00:00:00" & rdate< "2018-05-30 00:00:00"))$WTMP)
lay_bf2<-mean((filter(bf,rdate>"2018-03-15 00:00:00" & rdate< "2018-05-30 00:00:00"))$WTMP)
lay_oy2<-mean((filter(oy,rdate>"2018-05-01 00:00:00" & rdate< "2018-07-30 00:00:00"))$WTMP)
lay_wh2<-mean((filter(wh,rdate>"2018-07-01 00:00:00" & rdate< "2018-08-31 00:00:00"))$WTMP)
lay_gb2<-mean((filter(gbj,rdate>"2018-07-01 00:00:00" & rdate< "2018-08-31 00:00:00"))$WTMP)
lay_hm2<-mean((filter(hmi2,rdate>"2018-06-1 00:00:00" & rdate< "2018-07-15 00:00:00"))$WTMP)#inat
lay_wp2<-mean((filter(nah1516,rdate>"2018-06-1 00:00:00" & rdate< "2018-07-15 00:00:00"))$WTMP)#ruesink

#plot of site temperatures
#highlight regions of development
dev_dates<-read.csv(here::here('data/dev_dates.csv'))

dev_dates$xmin<-as.Date(dev_dates$xmin,format="%Y-%m-%d")
dev_dates$xmax<-as.Date(dev_dates$xmax,format="%Y-%m-%d")
dev_dates$ymin<--Inf
dev_dates$ymax<-Inf

dev_dates<-filter(dev_dates,site==c("Great Bay, NH","Skidaway, GA"))


ggplot(data=temp,aes(x=date,y=WTMP,color=site))+scale_fill_viridis_d(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),option="D")+scale_color_viridis_d(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),option="D")+theme_classic()+geom_vline(xintercept = 152)+geom_vline(xintercept = 273)+ylab("SST (°C)")+xlab("")+scale_x_date(date_labels = "%b")+theme(text=element_text(family="sanserif",size=14))+stat_summary(geom="line",fun.y=base::mean,size=2)+guides(color=guide_legend(override.aes=list(fill=NA)))

#north-south  development mean
ggplot(data=temp,aes(x=date,y=WTMP,color=site))+geom_rect(data=data_s,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=("grey"),inherit.aes=F)+geom_rect(data=dev_dates,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=site),alpha=0.2,inherit.aes=F)+scale_fill_manual(name="Site",labels=c("Great Bay, NH","Skidaway, GA"),values=c("yellow","blue"))+scale_color_viridis_d(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),option="D")+theme_classic()+geom_vline(xintercept = 152)+geom_vline(xintercept = 273)+ylab("SST (°C)")+xlab("")+scale_x_date(date_labels = "%b")+theme(text=element_text(family="sanserif",size=14))+stat_summary(geom="line",fun.y=base::mean,size=2)+guides(color=guide_legend(override.aes=list(fill=NA)))+
  geom_segment(aes(y=20,yend=20,x=as.Date("2018-03-01",format="%Y-%m-%d")),xend=as.Date("2018-05-30",format="%Y-%m-%d"),size=2,color="black")+geom_segment(aes(y=22.7,yend=22.7,x=as.Date("2018-07-01",format="%Y-%m-%d")),xend=as.Date("2018-08-31",format="%Y-%m-%d"),size=2,color="black")

#season length
gb_length<-gbj%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)
sk_length<-gcsk%>%mutate(day=as.Date(rdate,format="%Y-%m-%d"))%>%group_by(day)%>%dplyr::summarise(daily_wtmp=mean(WTMP))%>%na.omit()%>%filter(daily_wtmp>10)

data_s<-data.frame(xmin=as.Date("2018-01-01",format="%Y-%m-%d"),xmax=as.Date("2018-12-31",format="%Y-%m-%d"),ymin=-Inf,ymax=10)
ggplot(data=temp,aes(x=date,y=WTMP,color=site))+scale_fill_viridis_d(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),option="D")+scale_color_viridis_d(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),option="D")+theme_classic()+geom_vline(xintercept = 152)+geom_vline(xintercept = 273)+ylab("SST (°C)")+xlab("")+scale_x_date(date_labels = "%b")+theme(text=element_text(family="sanserif",size=14))+stat_summary(geom="line",fun.y=base::mean,size=2)+guides(color=guide_legend(override.aes=list(fill=NA)))+geom_rect(data=data_s,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=("grey"),alpha=0.85,inherit.aes=F)



##############################################################
#Contpour plot


pac3<-rbind(hmi2,nah1516,to3)
atll2<-rbind(gcsk,fb,bf,oy,wh,gbj)

temp_c<-rbind(pac3,atll2)
temp_c<-na.omit(temp_c)

temp_c$lat<-ifelse(temp_c$site=="bf",34.819,ifelse(temp_c$site=="fb",32.660525,ifelse(temp_c$site=="gbj",43.089589,
        ifelse(temp_c$site=="hmi2",40.849448,ifelse(temp_c$site=="oy",37.288562,ifelse(temp_c$site=="to3",38.12805,
        ifelse(temp_c$site=="wh",41.57687, ifelse(temp_c$site=="nah15",46.5007,ifelse(temp_c$site=="GCSK",31.970,NA)))))))))

temp_ca<-filter(temp_c,oce=="a")
temp_cp<-filter(temp_c,oce=="p")

ggplot(temp_ca,aes(x=date,y=lat))+geom_tile(aes(fill=WTMP),interpolate=T)+scale_fill_viridis_c()

temp_ca<-na.omit(temp_ca)
temp_cp<-na.omit(temp_cp)

dfi<-function(df=df){
  interp_df<-interp(x=temp_ca$date,y=temp_ca$lat,z=temp_ca$WTMP,duplicate="strip")
  interp2xyz(interp_df,data.frame=T)
}
dfip<-function(df=df){
  interp_df<-interp(x=temp_cp$date,y=temp_cp$lat,z=temp_cp$WTMP,duplicate="strip")
  interp2xyz(interp_df,data.frame=T)
}

ggg<-temp_ca%>%do(dfi(.))%>%tbl_df()%>%mutate(x=as_date(x,origin=lubridate::origin))
gggp<-temp_cp%>%do(dfip(.))%>%tbl_df()%>%mutate(x=as_date(x,origin=lubridate::origin))



ggplot(ggg,aes(x=x,y=y,z=z,fill=z))+geom_tile()+scale_fill_viridis_c(name="SST (C)")+stat_contour(color="black",bins=10)+xlab(NA)+ylab("Latitude")+geom_text_contour(aes(z=z))
ggplot(gggp,aes(x=x,y=y,z=z,fill=z))+geom_tile()+scale_fill_viridis_c(name="SST (C)")+xlab(NA)+ylab("Latitude")

ggg$oce<-"a"
gggp$oce<-"p"
try<-rbind(gggp,ggg)
oce.labs<-c("Pacific","Atlantic")
names(oce.labs)<-c("p","a")
try <- arrange(transform(try,
                           oce=factor(oce,levels=c("p","a"))),oce)
ggplot(try,aes(x=x,y=y,z=z,fill=z))+facet_wrap(oce~.,labeller=labeller(oce=oce.labs))+geom_raster()+scale_fill_viridis_c(name="SST (C)",option="B")+
  xlab(NA)+ylab("Latitude")+theme_classic()+
  theme(text=element_text(family="sanserif",size=14),axis.title.x=element_blank())+scale_x_date(labels=date_format("%b"))
                                               
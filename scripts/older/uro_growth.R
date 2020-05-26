library(knitr)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(forcats)
library(PerformanceAnalytics)
library(sjPlot)
library(lubridate)
library(HH)
library(AICcmodavg)
library(MuMIn)
library(segmented)
library(ggiraphExtra)
library(extrafont)
loadfonts(device = "win")


setwd("C:/Users/drewv/Documents/UMASS/data")
growth<-read.csv('growth.csv')
summary(growth)


which(growth$wt>0.09)
growth[92,12]=0.0109
growth[193,12]=.0151
growth[232,12]=.023
growth[268,12]=.0261


growth.alive<-growth[!(growth$alive=="n"),]

growth.alive<-growth.alive[!(growth$alive=="m"),]
growth.alive.con<-growth.alive[!(growth$ran.out=="1"),]
growth.alive.con<-na.omit(growth.alive.con)
growth.alive<-na.omit(growth.alive)

#growth.alive<-growth.alive[!(growth$oce=="NA"),]
mfrow=c(2,2)
ggplot(growth.alive,aes(x=temp,y=cal.length,color=pop))+geom_point()+geom_smooth()+facet_wrap(pop~.)
ggplot(growth.alive,aes(x=temp,y=cal.length,color=pop))+geom_point()+geom_smooth(se=FALSE)

ggplot(growth.alive.con,aes(x=temp,y=cal.length,color=pop))+geom_point()+geom_smooth()+facet_wrap(pop~.)
ggplot(growth.alive.con,aes(x=temp,y=cal.length,color=pop))+geom_point()+geom_smooth(se=FALSE)


ggplot(growth.alive,aes(x=temp,y=wt,color=pop))+geom_point()+geom_smooth()+facet_wrap(pop~.)
ggplot(growth.alive,aes(x=temp,y=wt,color=pop))+geom_point()+geom_smooth()


M <- (growth.alive[,9:10])
chart.Correlation(M,histogram=TRUE)

growth.alive<-na.omit(growth.alive)

##create dataframes for each population
bf<-filter(growth.alive,pop=="Beaufort")
fb<-filter(growth.alive,pop=="Folly Beach")
gb<-filter(growth.alive,pop=="Great Bay")
hm<-filter(growth.alive,pop=="Humboldt")
oy<-filter(growth.alive,pop=="Oyster")
sk<-filter(growth.alive,pop=="Skidaway")
wp<-filter(growth.alive,pop=="Willapa")
wh<-filter(growth.alive,pop=="Woods Hole")

models<-list(
  "null" = glm(cal.length~1,growth.alive,family=gaussian),
  "pop" = glm(cal.length~pop,growth.alive,family=gaussian),
  "temp" = glm(cal.length~temp,growth.alive,family=gaussian),
  "oce" = glm(cal.length~temp,growth.alive,family=gaussian),
  "bin" = glm(cal.length~bin,growth.alive,family=gaussian),
  "out" = glm(cal.length~ran.out,growth.alive,family=gaussian),
  
  "pop.temp" = glm(cal.length~pop+temp,growth.alive,family=gaussian),
  "pop.oce" = glm(cal.length~pop+oce,growth.alive,family=gaussian),
  "pop.bin" = glm(cal.length~pop+bin,growth.alive,family=gaussian),
  "pop.out" = glm(cal.length~pop+ran.out,growth.alive,family=gaussian),
  "temp.oce" = glm(cal.length~oce+temp,growth.alive,family=gaussian),
  "temp.bin" = glm(cal.length~bin+temp,growth.alive,family=gaussian),
  "oce.bin" = glm(cal.length~bin+oce,growth.alive,family=gaussian),
  "temp.out" = glm(cal.length~temp+ran.out,growth.alive,family=gaussian),
  "oce.out" = glm(cal.length~oce+ran.out,growth.alive,family=gaussian),
  "bin.out" = glm(cal.length~bin+ran.out,growth.alive,family=gaussian),
  
  "pop.temp.oce" = glm(cal.length~pop+temp+oce,growth.alive,family=gaussian),
  "pop.temp.bin" = glm(cal.length~pop+temp+bin,growth.alive,family=gaussian),
  "pop.temp.out" = glm(cal.length~pop+temp+ran.out,growth.alive,family=gaussian),
  "oce.temp.bin" = glm(cal.length~oce+temp+bin,growth.alive,family=gaussian),
  "oce.temp.out"= glm(cal.length~oce+temp+ran.out,growth.alive,family=gaussian),
  "oce.bin.out"=glm(cal.length~oce+bin+ran.out,growth.alive,family=gaussian),
  "bin.out.temp" = glm(cal.length~bin+ran.out+temp,growth.alive,family=gaussian),
  
  "pop.temp.oce.bin"= glm(cal.length~pop+oce+temp+bin,growth.alive,family=gaussian),
  "pop.temp.oce.out" = glm(cal.length~pop+oce+temp+ran.out,growth.alive,family=gaussian),
  "temp.oce.bin.out"= glm(cal.length~bin+oce+temp+ran.out,growth.alive,family=gaussian),
  
  "fulla" = glm(cal.length~pop+temp+oce+bin+ran.out,growth.alive,family=gaussian),
  
  
  "pop*temp" = glm(cal.length~pop*temp,growth.alive,family=gaussian),
  "pop*oce" = glm(cal.length~pop*oce,growth.alive,family=gaussian),
  "pop*bin" = glm(cal.length~pop*bin,growth.alive,family=gaussian),
  "temp*oce" = glm(cal.length~oce*temp,growth.alive,family=gaussian),
  "temp*bin" = glm(cal.length~bin*temp,growth.alive,family=gaussian),
  "oce*bin" = glm(cal.length~bin*oce,growth.alive,family=gaussian),
  "pop*out" = glm(cal.length~pop*ran.out,growth.alive,family=gaussian),
  "temp*out" = glm(cal.length~temp*ran.out,growth.alive,family=gaussian),
  "oce*out" = glm(cal.length~oce*ran.out,growth.alive,family=gaussian),
  "bin*out" = glm(cal.length~bin*ran.out,growth.alive,family=gaussian),
  
  "pop*temp*oce" = glm(cal.length~pop*temp*oce,growth.alive,family=gaussian),
  "pop*temp*bin" = glm(cal.length~pop*temp*bin,growth.alive,family=gaussian),
  "pop*temp*out" = glm(cal.length~pop*temp*ran.out,growth.alive,family=gaussian),
  "oce*temp*bin" = glm(cal.length~oce*temp*bin,growth.alive,family=gaussian),
  "oce*temp*out"= glm(cal.length~oce*temp*ran.out,growth.alive,family=gaussian),
  "oce*bin*out"=glm(cal.length~oce*bin*ran.out,growth.alive,family=gaussian),
  "bin*out*temp" = glm(cal.length~bin*ran.out*temp,growth.alive,family=gaussian),
  
  "pop*temp*oce*bin"= glm(cal.length~pop*oce*temp*bin,growth.alive,family=gaussian),
  "pop*temp*oce*out" = glm(cal.length~pop*oce*temp*ran.out,growth.alive,family=gaussian),
  "temp*oce*bin*out"= glm(cal.length~bin*oce*temp*ran.out,growth.alive,family=gaussian),
  
  "fulli" = glm(cal.length~pop*temp*oce*bin*ran.out,growth.alive,family=gaussian))

aictab(models)

summary(models$'pop*temp')
m1<-models$'pop*temp'
plot(m1)

#"pop*temp*oce" is the best model
##Extract residuals
hist(resid(m1),main="",xlab="residuals")
#normal as hell
growth.alive<-na.omit(growth.alive)
plot(resid(m1)~growth.alive$temp)
#resids for temperatures look a little wonky.
plot(resid(m1)~growth.alive$pop)
##Let's create a broken stick model for one population. I will have to create a model for that.

gb<-na.omit(gb)

m.wh<-glm(cal.length~temp,wh,family=gaussian)
seg.wh<-segmented(m.wh,seg.Z = ~temp, psi=29)

m.gb<-glm(cal.length~temp,gb,family=gaussian)
seg.gb<-segmented(m.gb,seg.Z = ~temp, psi=24)###hat's going on wrong in this line???)

m.oy<-glm(cal.length~temp,oy,family=gaussian)
seg.oy<-segmented(m.oy,seg.Z = ~temp, psi=24)

m.bf<-glm(cal.length~temp,bf,family=gaussian)
seg.bf<-segmented(m.bf,seg.Z = ~temp, psi=24)

m.fb<-glm(cal.length~temp,fb,family=gaussian)
seg.fb<-segmented(m.fb,seg.Z = ~temp, psi=24)

m.sk<-glm(cal.length~temp,sk,family=gaussian)
seg.sk<-segmented(m.sk,seg.Z = ~temp, psi=24)

m.hm<-glm(cal.length~temp,hm,family=gaussian)
seg.hm<-segmented(m.hm,seg.Z = ~temp, psi=24)

m.wp<-glm(cal.length~temp,wp,family=gaussian)
seg.wp<-segmented(m.wp,seg.Z = ~temp, psi=24)

xmin<-min(growth.alive$temp,na.rm=T)
xmax<-max(growth.alive$temp,na.rm=T)

all.length.seg<-list(seg.gb,seg.wh,seg.oy,seg.bf,seg.fb,seg.sk,seg.wp,seg.hm)


##plotting predicted values
predicted.gb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.gb$cal.length<-predict(seg.gb,predicted.gb)
predicted.gb$pop<-"gb"
predicted.gb$oce<-"a"

predicted.wh<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wh$cal.length<-predict(seg.wh,predicted.wh)
predicted.wh$pop<-"wh"
predicted.wh$oce<-"a"

predicted.oy<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.oy$cal.length<-predict(seg.oy,predicted.oy)
predicted.oy$pop<-"oy"
predicted.oy$oce<-"a"

predicted.bf<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.bf$cal.length<-predict(seg.bf,predicted.bf)
predicted.bf$pop<-"bf"
predicted.bf$oce<-"a"

predicted.fb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.fb$cal.length<-predict(seg.fb,predicted.fb)
predicted.fb$pop<-"fb"
predicted.fb$oce<-"a"

predicted.sk<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.sk$cal.length<-predict(seg.sk,predicted.sk)
predicted.sk$pop<-"sk"
predicted.sk$oce<-"a"

predicted.wp<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wp$cal.length<-predict(seg.wp,predicted.wp)
predicted.wp$pop<-"wp"
predicted.wp$oce<-"p"

predicted.hm<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.hm$cal.length<-predict(seg.hm,predicted.hm)
predicted.hm$pop<-"hm"
predicted.hm$oce<-"p"

all.length.pred<-rbind(predicted.gb,predicted.wh,predicted.oy,predicted.bf,predicted.fb,predicted.sk,predicted.wp,predicted.hm)
all.length.pred$pop<-factor(all.length.pred$pop,levels=c("gb","wh","oy","bf","fb","sk","wp","hm"))

ggplot(all.length.pred,aes(x=temp,y=cal.length))+geom_line(data=predicted.bf,size=1,color="yellow")+geom_line(data=predicted.oy,size=1,color="green")+
  geom_line(data=predicted.wh,size=1,color="blue")+geom_line(data=predicted.gb,size=1,color="purple")+geom_line(data=predicted.fb,size=1,color="orange")+geom_line(data=predicted.sk,size=1,color="red")+
  geom_line(data=predicted.wp,size=1,color="purple",linetype="dashed")+geom_line(data=predicted.hm,size=1,color="blue",linetype="dashed")

ggplot(all.length.pred,aes(x=temp,y=cal.length))+geom_line(aes(group=pop,x=temp,y=cal.length,color=pop,linetype=pop,size=pop))+
  ylab("Shell Length (mm)")+xlab("Common Garden Temperature (°C)")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  theme(text=element_text(family="arial",size=22))


ggplot(growth.alive,aes(x=temp,y=cal.length,group=interaction(pop,temp),fill=pop))+geom_boxplot()
#for nsf
nopac<-all.length.pred[!all.length.pred$oce=="p",]
ggplot(nopac,aes(x=temp,y=cal.length))+geom_line(aes(group=pop,x=temp,y=cal.length,color=pop,size=pop))+
  ylab("Growth (mm shell length)")+xlab("Temperature (°C)")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato"),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA"),name="Population")+
  theme(text=element_text(family="arial",size=22))

growth.alive<-growth.alive[!growth.alive$oce=="p",]
growth.alive<-growth.alive%>%
  mutate(pop = fct_relevel(pop,"Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway"))


ggplot(growth.alive,aes(x=temp,y=cal.length,group=pop,color=pop))+geom_point()+geom_smooth(method="loess",se=F,size=2)+
  geom_jitter()+theme_classic()+scale_color_discrete(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA"))+
  theme(text=element_text(family="arial",size=22))+ylab("Groth (mm)")+xlab("Common Garden Temperature (°C)")



########################################################
##What would happen if we eliminated the outliers from a few populations? EDIT-TPC collapse a little. 
growth.out<-growth.alive

ggplot(growth.out,aes(x=temp,y=cal.length,color=pop))+geom_point()+geom_smooth()+facet_wrap(pop~.)



growth.out<-growth.out[-c(100),]
growth.out<-growth.out[-c(304),]
growth.out<-growth.out[-c(143),]
growth.out<-growth.out[-c(166),]
growth.out<-na.omit(growth.out)
m1.1<-glm(cal.length~temp,growth.out,family=gaussian)

hist(resid(m1.1),main="",xlab="residuals")
#normal as hell
growth.out<-na.omit(growth.out)
plot(resid(m1.1)~growth.out$temp)
#resids for temperatures look a little wonky.
plot(resid(m1.1)~growth.out$pop)
#more wonky then if outliers kept

##New models
bf<-filter(growth.out, pop=="Beaufort")
fb<-filter(growth.out,pop=="Folly Beach")
gb<-filter(growth.out,pop=="Great Bay")
hm<-filter(growth.out,pop=="Humboldt")
oy<-filter(growth.out,pop=="Oyster")
sk<-filter(growth.out,pop=="Skidaway")
wp<-filter(growth.out,pop=="Willapa")
wh<-filter(growth.out,pop=="Woods Hole")

m.gb<-glm(cal.length~temp,gb,family=gaussian)
seg.gb<-segmented(m.gb,seg.z = ~temp, psi=24)

m.wh<-glm(cal.length~temp,wh,family=gaussian)
seg.wh<-segmented(m.wh,seg.z = ~temp, psi=24)

m.oy<-glm(cal.length~temp,oy,family=gaussian)
seg.oy<-segmented(m.oy,seg.Z = ~temp, psi=24)

m.bf<-glm(cal.length~temp,bf,family=gaussian)
seg.bf<-segmented(m.bf,seg.Z = ~temp, psi=24)

m.fb<-glm(cal.length~temp,fb,family=gaussian)
seg.fb<-segmented(m.fb,seg.Z = ~temp, psi=24)

m.sk<-glm(cal.length~temp,sk,family=gaussian)
seg.sk<-segmented(m.sk,seg.Z = ~temp, psi=24)

m.hm<-glm(cal.length~temp,hm,family=gaussian)
seg.hm<-segmented(m.hm,seg.Z = ~temp, psi=24)

m.wp<-glm(cal.length~temp,wp,family=gaussian)
seg.wp<-segmented(m.wp,seg.Z = ~temp, psi=24)

xmin<-min(growth.alive$temp,na.rm=T)
xmax<-max(growth.alive$temp,na.rm=T)

##plotting predicted values
predicted.gb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.gb$cal.length<-predict(seg.gb,predicted.gb)
predicted.gb$pop<-"gb"
predicted.gb$oce<-"a"

predicted.wh<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wh$cal.length<-predict(seg.wh,predicted.wh)
predicted.wh$pop<-"wh"
predicted.wh$oce<-"a"

predicted.oy<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.oy$cal.length<-predict(seg.oy,predicted.oy)
predicted.oy$pop<-"oy"
predicted.oy$oce<-"a"

predicted.bf<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.bf$cal.length<-predict(seg.bf,predicted.bf)
predicted.bf$pop<-"bf"
predicted.bf$oce<-"a"

predicted.fb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.fb$cal.length<-predict(seg.fb,predicted.fb)
predicted.fb$pop<-"fb"
predicted.fb$oce<-"fb"

predicted.sk<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.sk$cal.length<-predict(seg.sk,predicted.sk)
predicted.sk$pop<-"sk"
predicted.sk$oce<-"a"

predicted.wp<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wp$cal.length<-predict(seg.wp,predicted.wp)
predicted.wp$pop<-"wp"
predicted.wp$oce<-"p"

predicted.hm<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.hm$cal.length<-predict(seg.hm,predicted.hm)
predicted.hm$pop<-"hm"
predicted.hm$oce<-"p"

ggplot(growth.out,aes(x=temp,y=cal.length))+geom_line(data=predicted.bf,size=1,color="yellow")+geom_line(data=predicted.oy,size=1,color="green")+
  geom_line(data=predicted.wh,size=1,color="blue")+geom_line(data=predicted.gb,size=1,color="purple")+geom_line(data=predicted.fb,size=1,color="orange")+geom_line(data=predicted.sk,size=1,color="red")+
  geom_line(data=predicted.wp,size=1,color="purple",linetype="dashed")+geom_line(data=predicted.hm,size=1,color="blue",linetype="dashed")

all.length.pred<-rbind(predicted.gb,predicted.wh,predicted.oy,predicted.bf,predicted.fb,predicted.sk,predicted.wp,predicted.hm)
all.length.pred$pop<-factor(all.length.pred$pop,levels=c("gb","wh","oy","bf","fb","sk","wp","hm"))


ggplot(all.length.pred,aes(x=temp,y=cal.length))+geom_line(aes(group=pop,x=temp,y=cal.length,color=pop,linetype=pop),size=1.5)+
  ylab("Shell Length (mm)")+xlab("Common Garden Temperature (°C)")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,4,4),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population")+
  theme(text=element_text(family="arial",size=22))

ggplot(all.length.pred,aes(x=temp,y=cal.length))+geom_line(aes(group=pop,x=temp,y=cal.length,color=pop,linetype=pop),size=1.5)+
  ylab("Shell Length (mm)")+xlab("Common Garden Temperature (°C)")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,4,4),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_viridis_d(labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population",option="C")
ggplot(all.length.pred,aes(x=temp,y=cal.length))+geom_line(aes(group=pop,x=temp,y=cal.length,color=pop,linetype=pop),size=1.5)+
  ylab("Shell Length (mm)")+xlab("Common Garden Temperature (°C)")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,4,4),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_brewer(palette="Spectral",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt, CA"),name="Population")



##########################################################################################
###########################################################################################
#Extract breakpoints from growths
brkpts<-data.frame(matrix(,nrow=8,ncol=10))
colnames(brkpts)<-c("pop","brkptx","brkpty","lat","mean","s.mean","q.mean","t.mean","oce","ratio")

brkpts$pop<-list("Willapa","Humboldt","Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway")

brkpts[1,2]<-seg.wp$psi[[2]]
brkpts[2,2]<-seg.hm$psi[[2]]
brkpts[3,2]<-seg.gb$psi[[2]]
brkpts[4,2]<-seg.wh$psi[[2]]
brkpts[5,2]<-seg.oy$psi[[2]]
brkpts[6,2]<-seg.bf$psi[[2]]
brkpts[7,2]<-seg.fb$psi[[2]]
brkpts[8,2]<-seg.sk$psi[[2]]

brkpts[1,3]<-(seg.wp$psi[[2]]*coef(seg.wp)[[2]])+(coef(seg.wp)[[1]])
brkpts[2,3]<-(seg.hm$psi[[2]]*coef(seg.hm)[[2]])+(coef(seg.hm)[[1]])
brkpts[3,3]<-(seg.gb$psi[[2]]*coef(seg.gb)[[2]])+(coef(seg.gb)[[1]])
brkpts[4,3]<-(seg.wh$psi[[2]]*coef(seg.wh)[[2]])+(coef(seg.wh)[[1]])
brkpts[5,3]<-(seg.oy$psi[[2]]*coef(seg.oy)[[2]])+(coef(seg.oy)[[1]])
brkpts[6,3]<-(seg.bf$psi[[2]]*coef(seg.bf)[[2]])+(coef(seg.bf)[[1]])
brkpts[7,3]<-(seg.fb$psi[[2]]*coef(seg.fb)[[2]])+(coef(seg.fb)[[1]])
brkpts[8,3]<-(seg.sk$psi[[2]]*coef(seg.sk)[[2]])+(coef(seg.sk)[[1]])

#lat
brkpts$lat<-ifelse(brkpts$pop=="Beaufort",34.819,ifelse(brkpts$pop=="Folly Beach",32.660525,
                                                        ifelse(brkpts$pop=="Great Bay",43.089589,ifelse(brkpts$pop=="Humboldt",40.849448,ifelse(brkpts$pop=="Oyster",
                                                                                                                                                37.288562,ifelse(brkpts$pop=="Tomales",38.12805,ifelse(brkpts$pop=="Woods Hole",41.57687,ifelse(brkpts$pop=="Willapa",46.5007,ifelse(brkpts$pop=="Skidaway",31.970
                                                                                                                                                                                                                                                                                     ,NA)))))))))
#means
brkpts[1,5]<-means[1,1]
brkpts[2,5]<-means[3,1]
brkpts[3,5]<-means[5,1]
brkpts[4,5]<-means[8,1]
brkpts[5,5]<-means[7,1]
brkpts[6,5]<-means[6,1]
brkpts[7,5]<-means[9,1]
brkpts[8,5]<-means[4,1]

#s.mean
brkpts[1,6]<-s.mean[1,1]
brkpts[2,6]<-s.mean[3,1]
brkpts[3,6]<-s.mean[5,1]
brkpts[4,6]<-s.mean[8,1]
brkpts[5,6]<-s.mean[7,1]
brkpts[6,6]<-s.mean[6,1]
brkpts[7,6]<-s.mean[9,1]
brkpts[8,6]<-s.mean[4,1]
#q.mean
brkpts[1,7]<-q[6,2]
brkpts[2,7]<-q[7,2]
brkpts[3,7]<-q[1,2]
brkpts[4,7]<-q[2,2]
brkpts[5,7]<-q[3,2]
brkpts[6,7]<-q[4,2]
brkpts[7,7]<-q[5,2]
brkpts[8,7]<-quantile(s.gcsk$WTMP,0.75,type=1)
#t.mean
brkpts[1,8]<-q[6,3]
brkpts[2,8]<-q[7,3]
brkpts[3,8]<-q[1,3]
brkpts[4,8]<-q[2,3]
brkpts[5,8]<-q[3,3]
brkpts[6,8]<-q[4,3]
brkpts[7,8]<-q[5,3]
brkpts[8,8]<-quantile(s.gcsk$WTMP,0.9,type=1)

brkpts$oce<-ifelse(brkpts$pop=="Beaufort","a",ifelse(brkpts$pop=="Folly Beach","a",ifelse(brkpts$pop=="Great Bay","a",ifelse(brkpts$pop=="Humboldt","p",ifelse(brkpts$pop=="Oyster","a",ifelse(brkpts$pop=="Tomales","p",ifelse(brkpts$pop=="Woods Hole","a",ifelse(brkpts$pop=="Willapa","p",ifelse(brkpts$pop=="Skidaway","a",NA)))))))))

############################################################################
##Maximal trait performance (y)
mods.brk<-list(
  "null"=glm(brkpty~1,brkpts,family="gaussian"),
  "lat"=glm(brkpty~lat+oce,brkpts,family="gaussian"),
  "mean"=glm(brkpty~mean+oce,brkpts,family="gaussian"),
  "s.mean"=glm(brkpty~s.mean+oce,brkpts,family="gaussian"),
  "q.mean"=glm(brkpty~q.mean+oce,brkpts,family="gaussian"),
  "t.mean"=glm(brkpty~t.mean+oce,brkpts,family="gaussian"))

aictab(mods.brk)
summary(glm(brkpty~lat+oce,brkpts,family="gaussian"))
ggplot(brkpts,aes(x=lat,y=brkptx,color=lat,shape=oce))+geom_point(aes(size=2))+geom_smooth(method = 'lm')

##temp of breakpoint x size of breakpoint
ggplot(brkpts,aes(x=brkptx,y=brkpty,color=lat,size=lat))+geom_point()+geom_smooth(method = 'lm')

##Topt for NSF
brkptsnop<-brkpts
brkptsnop$oce<-as.factor(brkptsnop$oce)
#brkptsnop<-brkpts[!brkpts$oce=="p",]
ggplot(brkptsnop,aes(x=lat,y=brkpty,color=oce,fill=oce),group=oce)+
  geom_point(size=3)+theme_classic()+
  ylab("Maximal Trait Performance (mm)")+
  xlab("Latitude (degrees)")+
  theme(text=element_text(family="arial",size=22))+
    scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
    scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  geom_abline(intercept=0.94555,slope=0.0888,size=2,color='#F8766D')+scale_x_continuous(limits=c(30,48),breaks=c(30,34,38,42,46))+
  scale_y_continuous(breaks=c(3.5,4,4.5,5,5.5),limits=c(3.5,5.5))

#brkptsnop<-brkptsnop[-c(2),]

summary(glm(brkpty~lat+oce,data=brkptsnop))
summary(glm(brkpty~lat,data=brkptsnop))

#############################################################################
##Topt (x)
mods.brkx<-list(
  "null"=glm(brkptx~1,brkpts,family="gaussian"),
  "lat"=glm(brkptx~lat+oce,brkpts,family="gaussian"),
  "mean"=glm(brkptx~mean+oce,brkpts,family="gaussian"),
  "s.mean"=glm(brkptx~s.mean+oce,brkpts,family="gaussian"),
  "q.mean"=glm(brkptx~q.mean+oce,brkpts,family="gaussian"),
  "t.mean"=glm(brkptx~t.mean+oce,brkpts,family="gaussian"),
  "lat*oce"=glm(brkptx~lat*oce,brkpts,family="gaussian"),
  "oce*mean"=glm(brkptx~mean*oce,brkpts,family="gaussian"),
  "oce*s.mean"=glm(brkptx~s.mean*oce,brkpts,family="gaussian"),
  "oce*q.mean"=glm(brkptx~q.mean*oce,brkpts,family="gaussian"),
  "oce*t.mean"=glm(brkptx~t.mean*oce,brkpts,family="gaussian"))
aictab(mods.brkx)

summary(glm(brkptx~lat+oce,brkpts,family="gaussian"))

ggplot(brkpts,aes(x=lat,y=brkptx,color=oce,fill=oce),group=oce)+
  geom_point(size=3)+theme_classic()+
  ylab("Thermal Optima")+
  xlab("Latitude (degrees)")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))

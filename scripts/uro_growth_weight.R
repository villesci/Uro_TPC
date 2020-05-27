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

###eliminate snails that ever ran out of food
growth<-subset(growth,code!=(6833)&code!=(6831)&code!=(6711)&code!=(6632)&code!=(6631)&code!=(6611)&code!=(6433)&code!=(6422)&code!=(6412)&code!=(6411)&code!=(6233)&code!=(6212)&code!=(6212)&code!=(6111)&code!=(5831)&code!=(5813)&code!=(5732)&code!=(5731)&code!=(5723)&code!=(5711)&code!=(5622)&code!=(5531)&code!=(5333)&code!=(5313)&code!=(5312)&code!=(5213)&code!=(5121)&code!=(5112)&code!=(4522)&code!=(4521)&code!=(4433)&code!=(4432)&code!=(4231)&code!=(3821)&code!=(3712)&code!=(3433)&code!=(3432)&code!=(3421)&code!=(3413)&code!=(2422)&code!=(2421))


growth.alive<-growth[!(growth$alive=="n"),]

growth.alive<-growth.alive[!(growth$alive=="m"),]
growth.alive.con<-growth.alive[!(growth$ran.out=="1"),]
growth.alive.con<-na.omit(growth.alive.con)
growth.alive<-na.omit(growth.alive)

weight.model<-glm(wt~pop*temp*oce*bin*ran.out,growth.alive,family=gaussian)
plot(weight.model)
hist(resid(weight.model),breaks=50)
#normal, no transformations
###################################################################################
#####################################################################################
##What if we did growth.alive, but with weight?
bf<-filter(growth.alive, pop=="Beaufort")
fb<-filter(growth.alive,pop=="Folly Beach")
gb<-filter(growth.alive,pop=="Great Bay")
hm<-filter(growth.alive,pop=="Humboldt")
oy<-filter(growth.alive,pop=="Oyster")
sk<-filter(growth.alive,pop=="Skidaway")
wp<-filter(growth.alive,pop=="Willapa")
wh<-filter(growth.alive,pop=="Woods Hole")

m1.2<-glm(wt~temp*pop,growth.alive,family=gaussian)
#"pop*temp*oce" is the best model
##Extract residuals
hist(resid(m1.2),main="",xlab="residuals")
#normal as hell
growth.alive<-na.omit(growth.alive)
plot(resid(m1.2)~growth.alive$temp)
#resids for temperatures look a little wonky.
plot(resid(m1.2)~growth.alive$pop)

m.gb<-glm(wt~temp,gb,family=gaussian)
seg.gb<-segmented(m.gb,seg.z = ~temp, psi=24)

m.wh<-glm(wt~temp,wh,family=gaussian)
seg.wh<-segmented(m.wh,seg.z = ~temp, psi=24)

m.oy<-glm(wt~temp,oy,family=gaussian)
seg.oy<-segmented(m.oy,seg.Z = ~temp, psi=26,seg.control(maxit.glm=100),fix.npsi=T,n.boot=500)

m.bf<-glm(wt~temp,bf,family=gaussian)
seg.bf<-segmented(m.bf,seg.Z = ~temp, psi=24)

m.fb<-glm(wt~temp,fb,family=gaussian)
seg.fb<-segmented(m.fb,seg.Z = ~temp, psi=24)

m.sk<-glm(wt~temp,sk,family=gaussian)
seg.sk<-segmented(m.sk,seg.Z = ~temp, psi=24)

m.hm<-glm(wt~temp,hm,family=gaussian)
seg.hm<-segmented(m.hm,seg.Z = ~temp, psi=24)

m.wp<-glm(wt~temp,wp,family=gaussian)
seg.wp<-segmented(m.wp,seg.Z = ~temp, psi=24)

xmin<-min(growth.alive$temp,na.rm=T)
xmax<-max(growth.alive$temp,na.rm=T)

##plotting predicted values
predicted.gb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.gb$wt<-predict(seg.gb,predicted.gb)
predicted.gb$pop<-"gb"
predicted.gb$oce<-"a"

predicted.wh<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wh$wt<-predict(seg.wh,predicted.wh)
predicted.wh$pop<-"wh"
predicted.wh$oce<-"a"

predicted.oy<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.oy$wt<-predict(seg.oy,predicted.oy)
predicted.oy$pop<-"oy"
predicted.oy$oce<-"a"

predicted.bf<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.bf$wt<-predict(seg.bf,predicted.bf)
predicted.bf$pop<-"bf"
predicted.bf$oce<-"a"

predicted.fb<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.fb$wt<-predict(seg.fb,predicted.fb)
predicted.fb$pop<-"fb"
predicted.fb$oce<-"a"

predicted.sk<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.sk$wt<-predict(seg.sk,predicted.sk)
predicted.sk$pop<-"sk"
predicted.sk$oce<-"a"

predicted.wp<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.wp$wt<-predict(seg.wp,predicted.wp)
predicted.wp$pop<-"wp"
predicted.wp$oce<-"p"


predicted.hm<-data.frame(temp=seq(xmin,xmax,length.out=100))
predicted.hm$wt<-predict(seg.hm,predicted.hm)
predicted.hm$pop<-"hm"
predicted.hm$oce<-"p"

all.pred<-rbind(predicted.gb,predicted.wh,predicted.oy,predicted.bf,predicted.fb,predicted.sk,predicted.wp,predicted.hm)

all.pred$pop<-factor(all.pred$pop,levels=c("gb","wh","oy","bf","fb","sk","wp","hm"))
all.pred$size<-1
all.pred$population<-ifelse(all.pred$pop=="gb","Great Bay",ifelse(all.pred$pop=="wh","Woods Hole",ifelse(all.pred$pop=="oy","Oyster",ifelse(all.pred$pop=="bf","Beaufort",ifelse(all.pred$pop=="fb","Folly Beach",ifelse(all.pred$pop=="sk","Skidaway",ifelse(all.pred$pop=="wp","Willapa",ifelse(all.pred$pop=="hm","Humboldt",NA))))))))
all.pred$population<-as.factor(all.pred$population)
all.pred$population<-factor(all.pred$population,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))


growth.alive$population<-growth.alive$pop
growth.alive$population<-factor(growth.alive$pop,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))

ggplot(all.pred,aes(x=temp,y=wt,group=pop))+geom_line(aes(x=temp,y=wt,color=pop,linetype=pop,size=pop))+
  ylab("Weight(g)")+xlab("Common Garden Temperature")+theme_classic()+
  scale_y_continuous(breaks=c(0.005,0.01,0.015,0.02,0.025))+scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")

ggplot(all.pred,aes(x=temp,y=wt,group=population))+geom_line(aes(x=temp,y=wt,color=population,linetype=population,size=population))+
  ylab("Weight(g)")+xlab("Common Garden Temperature")+theme_classic()+
  scale_y_continuous(breaks=c(0.005,0.01,0.015,0.02,0.025))+scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="population")+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="population")+
  geom_point(data=growth.alive,aes(x=temp,y=wt,group=population))+facet_wrap(population~.)+theme(legend.position = "none")


ggplot(growth.alive,aes(x=temp,y=wt,color=pop))+geom_point()+geom_smooth()+facet_wrap(pop~.)+ylab("Weight (g)")+xlab("Common Garden Temperature")
##interesting...more stacked!
ggplot(growth.alive,aes(x=temp,y=wt,group=interaction(pop,temp),fill=pop))+geom_boxplot()+facet_wrap(pop~.)

###are the starting caliper lengths different amongst populations?
fit<-aov(cal.length.start~pop,data=growth.alive)
summary(fit)
plot(residuals(fit))
hist(resid(fit))
#AnOVA says populations has no effect, but let's do a tukey
TukeyHSD(fit)

########################################################################################
#broken stick
#Extract breakpoints from growths
brkpts<-data.frame(matrix(,nrow=8,ncol=13))
colnames(brkpts)<-c("pop","brkptx","brkpty","lat","mean","s.mean","q.mean","t.mean","oce","ratio","brkptxq","brkptyq","max")

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
brkpts[2,5]<-means[1,1]
brkpts[8,5]<-means[3,1]
brkpts[6,5]<-means[5,1]
brkpts[3,5]<-means[8,1]
brkpts[4,5]<-means[7,1]
brkpts[5,5]<-means[6,1]
brkpts[1,5]<-means[2,1]
brkpts[7,5]<-means[4,1]

#s.mean
brkpts[2,6]<-s.mean[1,1]
brkpts[8,6]<-s.mean[3,1]
brkpts[6,6]<-s.mean[5,1]
brkpts[3,6]<-s.mean[8,1]
brkpts[4,6]<-s.mean[7,1]
brkpts[5,6]<-s.mean[6,1]
brkpts[1,6]<-s.mean[2,1]
brkpts[7,6]<-s.mean[4,1]
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
#max
brkpts[1,13]<-q[6,4]
brkpts[2,13]<-q[7,4]
brkpts[3,13]<-q[1,4]
brkpts[4,13]<-q[2,4]
brkpts[5,13]<-q[3,4]
brkpts[6,13]<-q[4,4]
brkpts[7,13]<-q[5,4]
brkpts[8,13]<-q[9,4]

brkpts$oce<-ifelse(brkpts$pop=="Beaufort","a",ifelse(brkpts$pop=="Folly Beach","a",ifelse(brkpts$pop=="Great Bay","a",ifelse(brkpts$pop=="Humboldt","p",ifelse(brkpts$pop=="Oyster","a",ifelse(brkpts$pop=="Tomales","p",ifelse(brkpts$pop=="Woods Hole","a",ifelse(brkpts$pop=="Willapa","p",ifelse(brkpts$pop=="Skidaway","a",NA)))))))))

############################################################################
##Maximal trait performance (y)
mods.brk<-list(
  "null"=glm(brkpty~1,brkpts,family="gaussian"),
  "lat"=glm(brkpty~lat+oce,brkpts,family="gaussian"),
  "mean"=glm(brkpty~mean+oce,brkpts,family="gaussian"),
  "s.mean"=glm(brkpty~s.mean+oce,brkpts,family="gaussian"),
  "q.mean"=glm(brkpty~q.mean+oce,brkpts,family="gaussian"),
  "t.mean"=glm(brkpty~t.mean+oce,brkpts,family="gaussian"),
  "oce*max"=glm(brkpty~max*oce,brkpts,family="gaussian"),
  "max"=glm(brkpty~max+oce,brkpts,family="gaussian"),
  "oce*lat"=glm(brkpty~lat*oce,brkpts,family="gaussian"),
  "oce*mean"=glm(brkpty~mean*oce,brkpts,family="gaussian"),
  "oce*s.mean"=glm(brkpty~s.mean*oce,brkpts,family="gaussian"),
  "oce*q.mean"=glm(brkpty~q.mean*oce,brkpts,family="gaussian"))

  

aictab(mods.brk)
summary(glm(brkpty~lat+oce,brkpts,family="gaussian"))
ggplot(brkpts,aes(x=lat,y=brkptx,color=lat,shape=oce))+geom_point(aes(size=2))+geom_smooth(method = 'lm')

##temp of breakpoint x size of breakpoint
ggplot(brkpts,aes(x=brkptx,y=brkpty,color=lat,size=lat))+geom_point()+geom_smooth(method = 'lm')

##Topt for NSF
brkptsnop<-brkpts
brkptsnop$oce<-as.factor(brkptsnop$oce)
#brkptsnop<-brkpts[!brkpts$oce=="p",]
ggplot(brkptsnop,aes(x=t.mean,y=brkpty,color=oce,fill=oce),group=oce)+
  geom_point(size=3)+theme_classic()+
  ylab("Maximal Trait Performance (g)")+
  xlab("Latitude (degrees)")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+geom_smooth(aes(group=oce),method="lm",se=F)


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
  "oce*t.mean"=glm(brkptx~t.mean*oce,brkpts,family="gaussian"),
  "oce*max"=glm(brkptx~max*oce,brkpts,family="gaussian"),
  "max"=glm(brkptx~max+oce,brkpts,family="gaussian"))
  
aictab(mods.brkx)




summary(glm(brkptx~mean+oce,brkpts,family="gaussian"))

ggplot(brkpts,aes(x=mean,y=brkptx,color=oce,fill=oce),group=oce)+
  geom_point(size=3)+theme_classic()+
  ylab("Thermal Optima")+
  xlab("Mean")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+geom_smooth(aes(group=oce),method="lm",se=F)

#####################################################################################
#################################################################################
##Quadratic

quadm<-(glm(wt~poly(growth.alive$temp,2)+pop,growth.alive,family="gaussian"))
hist(resid(quadm))
plot(quadm)

growth.alive$quad<-poly(growth.alive$temp,2)
bf<-filter(growth.alive,pop=="Beaufort")
fb<-filter(growth.alive,pop=="Folly Beach")
gb<-filter(growth.alive,pop=="Great Bay")
hm<-filter(growth.alive,pop=="Humboldt")
oy<-filter(growth.alive,pop=="Oyster")
sk<-filter(growth.alive,pop=="Skidaway")
wp<-filter(growth.alive,pop=="Willapa")
wh<-filter(growth.alive,pop=="Woods Hole")

xminq<-min(poly(growth.alive$temp,2),na.rm=T)
xmaxq<-max(poly(growth.alive$temp,2),na.rm=T)

qmgb<-(lm(wt~poly(gb$temp,2,raw=T),gb))
cfgb<-coef(qmgb)
brkpts[3,11]<-(-cfgb[2]/(2*(cfgb[3])))
brkpts[3,12]<-cfgb[1]+cfgb[2]*brkpts[3,11]+cfgb[3]*(brkpts[3,11]^2)

qmwh<-(lm(wt~poly(wh$temp,2,raw=T),wh))
cfwh<-coef(qmwh)
brkpts[4,11]<-(-cfwh[2]/(2*(cfwh[3])))
brkpts[4,12]<-cfwh[1]+cfwh[2]*brkpts[4,11]+cfwh[3]*(brkpts[4,11]^2)

qmoy<-(lm(wt~poly(oy$temp,2,raw=T),oy))
cfoy<-coef(qmoy)
brkpts[5,11]<-(-cfoy[2]/(2*(cfoy[3])))
brkpts[5,12]<-cfoy[1]+cfoy[2]*brkpts[5,11]+cfoy[3]*(brkpts[5,11]^2)

qmbf<-(lm(wt~poly(bf$temp,2,raw=T),bf))
cfbf<-coef(qmbf)
brkpts[6,11]<-(-cfbf[2]/(2*(cfbf[3])))
brkpts[6,12]<-cfbf[1]+cfbf[2]*brkpts[6,11]+cfbf[3]*(brkpts[6,11]^2)


qmfb<-(lm(wt~poly(fb$temp,2,raw=T),fb))
cffb<-coef(qmfb)
brkpts[7,11]<-(-cffb[2]/(2*(cffb[3])))
brkpts[7,12]<-cffb[1]+cffb[2]*brkpts[7,11]+cffb[3]*(brkpts[7,11]^2)

qmsk<-(lm(wt~poly(sk$temp,2,raw=T),sk))
cfsk<-coef(qmsk)
brkpts[8,11]<-(-cfsk[2]/(2*(cfsk[3])))
brkpts[8,12]<-cfsk[1]+cfsk[2]*brkpts[8,11]+cfsk[3]*(brkpts[8,11]^2)

qmwp<-(lm(wt~poly(wp$temp,2,raw=T),wp))
cfwp<-coef(qmwp)
brkpts[1,11]<-(-cfwp[2]/(2*(cfwp[3])))
brkpts[1,12]<-cfwp[1]+cfwp[2]*brkpts[1,11]+cfwp[3]*(brkpts[1,11]^2)

qmhm<-(lm(wt~poly(hm$temp,2,raw=T),hm))
cfhm<-coef(qmhm)
brkpts[2,11]<-(-cfhm[2]/(2*(cfhm[3])))
brkpts[2,12]<-cfhm[1]+cfhm[2]*brkpts[2,11]+cfhm[3]*(brkpts[2,11]^2)


growth.alive$population<-factor(growth.alive$pop,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))


ggplot(growth.alive,aes(y=wt,x=temp,group=population,linetype=population))+geom_smooth(method='lm',formula=y~poly(x,2),se=F,aes(color=population,size=population))+geom_point()+facet_wrap(population~.)+
  theme_classic()+theme(legend.position = "none")+labs(y="Weight (g)",x="Temperature (?C)")+scale_y_continuous(breaks=c(0.005,0.01,0.015,0.02,0.025))+scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="population")+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="population")+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")

ggplot(growth.alive,aes(y=wt,x=temp,group=population,color=population,linetype=population))+geom_smooth(method='lm',formula=y~poly(x,2),se=F,aes(color=population,size=population))+
  scale_y_continuous(breaks=c(0.005,0.01,0.015,0.02,0.025))+scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  theme_classic()+labs(y="Weight (g)",x="Temperature (?C)")


ggplot(oy,aes(y=wt,x=temp))+geom_smooth(method='lm',formula=y~poly(x,2),se=F)+geom_point()

ggplot(predicted.oy,aes(x=temp,y=wt))+geom_line(aes(x=temp,y=wt))

#brkpts<-filter(brkpts,pop!="Oyster")

mods.brkyq<-list(
  "null"=glm(brkptyq~1,brkpts,family="gaussian"),
  "lat"=glm(brkptyq~lat+oce,brkpts,family="gaussian"),
  "mean"=glm(brkptyq~mean+oce,brkpts,family="gaussian"),
  "s.mean"=glm(brkptyq~s.mean+oce,brkpts,family="gaussian"),
  "q.mean"=glm(brkptyq~q.mean+oce,brkpts,family="gaussian"),
  "t.mean"=glm(brkptyq~t.mean+oce,brkpts,family="gaussian"),
  "*lat"=glm(brkptyq~lat*oce,brkpts,family="gaussian"),
  "*mean"=glm(brkptyq~mean*oce,brkpts,family="gaussian"),
  "*s.mean"=glm(brkptyq~s.mean*oce,brkpts,family="gaussian"),
  "*q.mean"=glm(brkptyq~q.mean*oce,brkpts,family="gaussian"),
  "*t.mean"=glm(brkptyq~t.mean*oce,brkpts,family="gaussian")
)

aictab(mods.brkyq)

brkptxmq<-glm(brkptyq~lat+oce,brkpts,family="gaussian")
summary(brkptxmq)

ggplot(brkpts,aes(x=lat,y=brkptyq,color=oce),group=interaction(oce))+
  geom_point(size=3)+geom_smooth(aes(group=oce),method="lm",se=F)+theme_classic()+
  ylab("Maximal Growth (g)")+
  xlab("Latitude (degrees)")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))

mods.brkxq<-list(
  "null"=glm(brkptxq~1,brkpts,family="gaussian"),
  "lat"=glm(brkptxq~lat+oce,brkpts,family="gaussian"),
  "mean"=glm(brkptxq~mean+oce,brkpts,family="gaussian"),
  "s.mean"=glm(brkptxq~s.mean+oce,brkpts,family="gaussian"),
  "q.mean"=glm(brkptxq~q.mean+oce,brkpts,family="gaussian"),
  "t.mean"=glm(brkptxq~t.mean+oce,brkpts,family="gaussian"),
  "*lat"=glm(brkptxq~lat*oce,brkpts,family="gaussian"),
  "*mean"=glm(brkptxq~mean*oce,brkpts,family="gaussian"),
  "*s.mean"=glm(brkptxq~s.mean*oce,brkpts,family="gaussian"),
  "*q.mean"=glm(brkptxq~q.mean*oce,brkpts,family="gaussian"),
  "*t.mean"=glm(brkptxq~t.mean*oce,brkpts,family="gaussian")
)

aictab(mods.brkxq)

brkptxmq<-glm(brkptx~lat+oce,brkpts,family="gaussian")
summary(brkptxmq)

ggplot(brkpts,aes(x=lat,y=brkptxq,color=oce),group=interaction(oce))+
  geom_point(size=3)+geom_smooth(aes(group=oce),method="lm",se=F)+theme_classic()+
  ylab("Thermal Optima")+
  xlab("Latitude (degrees)")+
  theme(text=element_text(family="arial",size=22))+
  scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
  scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))



brkpts$pop<-unlist(brkpts$pop)
write.csv(brkpts,"C:/Users/drewv/Documents/UMASS/data/growth_wt.csv",row.names=F)


##consumption##
setwd('C:/Users/drewv/Documents/UMASS/data/')
library(tidyr)
library(dplyr)
library(segmented)

uro.con<-read.csv('uro.consumption.csv',header=T)

uro.con<-na.omit(uro.con)
uro.con$no.consumed<-as.numeric(uro.con$no.consumed)

View(uro.con)
summary(uro.con)

hist(uro.con$no.consumed)


hist(uro.con$rate)


plot(uro.con$rate~uro.con$Population)
plot(uro.con$rate~uro.con$Temp)


ggplot(uro.con,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+facet_wrap(Temp~.)+geom_boxplot(aes(fill=Population))
ggplot(uro.con,aes(x=Temp,y=rate,group=interaction(Population,Temp)))+facet_wrap(timepoint~.)+geom_boxplot(aes(fill=Population))

uro.con.16<-filter(uro.con,Temp=="16")
uro.con.20<-filter(uro.con,Temp=="20")
uro.con.24<-filter(uro.con,Temp=="24")
uro.con.26<-filter(uro.con,Temp=="26")
uro.con.28<-filter(uro.con,Temp=="28")
uro.con.30<-filter(uro.con,Temp=="30")
ggplot(uro.con.16,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+geom_boxplot(aes(fill=Population))
ggplot(uro.con.28,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+geom_boxplot(aes(fill=Population))

  
full <-glm(rate ~ Population *Temp * all.consumed * tank.replicate, uro.con, family = gaussian)
plot(full)
hist(resid(full))

library(AICcmodavg)
library(MuMIn)
models<-list("null" = glm(rate~1,uro.con,family=gaussian),
             "pop"= glm(rate~Population,uro.con,family=gaussian),
             "temp" = glm(rate~Temp,uro.con,family=gaussian),
             "all.consumed" = glm(rate~all.consumed,uro.con,family=gaussian),
             "tank.rep" = glm(rate~tank.replicate,uro.con,family=gaussian),
             "full" = glm(rate ~ Population + Temp + all.consumed + tank.replicate, uro.con, family = gaussian),
             "P+T+C"=glm(rate ~ Population + Temp + all.consumed, uro.con, family = gaussian),
             "P+T+R"=glm(rate ~ Population + Temp + tank.replicate, uro.con, family = gaussian),
             "P+C+R" = glm(rate ~ Population + all.consumed + tank.replicate, uro.con, family = gaussian),
             "T+R+C"=glm(rate ~ Temp + all.consumed + tank.replicate, uro.con, family = gaussian),
             "P+T"=glm(rate ~ Population + Temp, uro.con, family = gaussian),
             "P+C" = glm(rate ~ Population + all.consumed, uro.con, family = gaussian),
             "P+R"=glm(rate ~ Population + tank.replicate, uro.con, family = gaussian),
             "T+C"=glm(rate ~ Temp + all.consumed, uro.con, family = gaussian),
             "T+R"=glm(rate ~ Temp + tank.replicate, uro.con, family = gaussian),
             "C+R"=glm(rate ~ all.consumed + tank.replicate, uro.con, family = gaussian),
             "P+T*C"=glm(rate ~ Population + Temp * all.consumed, uro.con, family = gaussian),
             "fulli" = glm(rate ~ Population *Temp * all.consumed * tank.replicate, uro.con, family = gaussian),
             "P*T*C"=glm(rate ~ Population * Temp * all.consumed, uro.con, family = gaussian),
             "P*T*R"=glm(rate ~ Population * Temp * tank.replicate, uro.con, family = gaussian),
             "P*C*R" = glm(rate ~ Population * all.consumed * tank.replicate, uro.con, family = gaussian),
             "T*R*C"=glm(rate ~ Temp * all.consumed * tank.replicate, uro.con, family = gaussian),
             "P*T"=glm(rate ~ Population * Temp, uro.con, family = gaussian),
             "P*C" = glm(rate ~ Population * all.consumed, uro.con, family = gaussian),
             "P*R"=glm(rate ~ Population * tank.replicate, uro.con, family = gaussian),
             "T*C"=glm(rate ~ Temp * all.consumed, uro.con, family = gaussian),
             "T*R"=glm(rate ~ Temp * tank.replicate, uro.con, family = gaussian),
             "C*R"=glm(rate ~ all.consumed * tank.replicate, uro.con, family = gaussian))
aictab(models)
model.sel(models)
##Population + Temperature + consumed
m1<-models$'P+T*C'
plot(m1)
hist(resid(m1))

##LOOPS
pops<-unique(uro.con$Population)
temps<-unique(uro.con$Temp)
matrix<-matrix(nrow=length(pops),ncol=length(temps))
tmp2<-filter(uro.con,Temp==16, Population=="Willapa")

m<-matrix(ncol=length(uro.con),nrow=54)

df<-array(0,dim=c(length(pops),length(temps),length(m)),dimnames=list(pops,temps,m))
#df<-array(0,dim=c(length(pops),length(temps),matrix),dimnames=list(pops,temps,matrix))
for (i in 1:length(pops)){
  for(j in 1:length(temps)){
  m[[i]] <- filter(uro.con,Population==pops[i], Temp==temps[j])
  }
}
  

modlist<-list()
for (i in 1:length(df)){
  mod<-glm(data=df[[i]],rate~Population+Temp*all.consumed,family="gaussian")
  modlist[[i]]<-mod
}
names(modlist)<-pops

######################################################################
######################################################################
uro.con2<-filter(uro.con,timepoint=="3")
ggplot(uro.con2,aes(x=Temp,y=rate))+geom_jitter()+geom_smooth()+facet_wrap(Population~.)
bf<-filter(uro.con2,Population=="Beaufort")
fb<-filter(uro.con2,Population=="Folly Beach")
gb<-filter(uro.con2,Population=="Great Bay")
hm<-filter(uro.con2,Population=="Humboldt")
oy<-filter(uro.con2,Population=="Oyster")
sk<-filter(uro.con2,Population=="Skidaway")
wp<-filter(uro.con2,Population=="Willapa")
wh<-filter(uro.con2,Population=="Woods Hole")

m.wh<-glm(rate~Temp,wh,family=gaussian)
seg.wh<-segmented(m.wh,seg.Z = ~Temp, psi=24)

m.gb<-glm(rate~Temp,gb,family=gaussian)
seg.gb<-segmented(m.gb,seg.Z = ~Temp, psi=24)

m.oy<-glm(rate~Temp,oy,family=gaussian)
seg.oy<-segmented(m.oy,seg.Z = ~Temp, psi=24)

m.bf<-glm(rate~Temp,bf,family=gaussian)
seg.bf<-segmented(m.bf,seg.Z = ~Temp, psi=29,seg.control(maxit.glm=10),fix.npsi=T,n.boot=50)

m.fb<-glm(rate~Temp,fb,family=gaussian)
seg.fb<-segmented(m.fb,seg.Z = ~Temp, psi=24)

m.sk<-glm(rate~Temp,sk,family=gaussian)
seg.sk<-segmented(m.sk,seg.Z = ~Temp, psi=24)

m.hm<-glm(rate~Temp,hm,family=gaussian)
seg.hm<-segmented(m.hm,seg.Z = ~Temp, psi=24)

m.wp<-glm(rate~Temp,wp,family=gaussian)
seg.wp<-segmented(m.wp,seg.Z = ~Temp, psi=24)

xmin<-min(uro.con2$Temp,na.rm=T)
xmax<-max(uro.con2$Temp,na.rm=T)

all.length.seg<-list(seg.gb,seg.wh,seg.oy,seg.bf,seg.fb,seg.sk,seg.wp,seg.hm)


##plotting predicted values
predicted.gb<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.gb$rate<-predict(seg.gb,predicted.gb)
predicted.gb$pop<-"gb"
predicted.gb$oce<-"a"

predicted.wh<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.wh$rate<-predict(seg.wh,predicted.wh)
predicted.wh$pop<-"wh"
predicted.wh$oce<-"a"

predicted.oy<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.oy$rate<-predict(seg.oy,predicted.oy)
predicted.oy$pop<-"oy"
predicted.oy$oce<-"a"

predicted.bf<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.bf$rate<-predict(seg.bf,predicted.bf)
predicted.bf$pop<-"bf"
predicted.bf$oce<-"a"

#iterative searching for bf
breaks<-bf$Temp[which(bf$Temp>=22 & bf$Temp<=29)]
mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
  piecewise1 <- lm(rate ~ Temp*(Temp < breaks[i]) + Temp*(Temp>=breaks[i]),bf)
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)
breaks[which(mse==min(mse))]
piecewise2 <- lm(rate ~ Temp*(Temp < 26) + Temp*(Temp > 26),bf)
summary(piecewise2)
plot(rate~Temp,bf)
curve((-3.2222+1.5284)+(0.2222-0.0963)*x,add=T,from=16,to=26)
curve((-3.2222-.7407)+0.2222*x,add=T,from=26,to=30)

predicted.fb<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.fb$rate<-predict(seg.fb,predicted.fb)
predicted.fb$pop<-"fb"
predicted.fb$oce<-"a"

predicted.sk<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.sk$rate<-predict(seg.sk,predicted.sk)
predicted.sk$pop<-"sk"
predicted.sk$oce<-"a"

predicted.wp<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.wp$rate<-predict(seg.wp,predicted.wp)
predicted.wp$pop<-"wp"
predicted.wp$oce<-"p"

predicted.hm<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.hm$rate<-predict(seg.hm,predicted.hm)
predicted.hm$pop<-"hm"
predicted.hm$oce<-"p"

all.cons.pred<-rbind(predicted.gb,predicted.wh,predicted.oy,predicted.bf,predicted.fb,predicted.sk,predicted.wp,predicted.hm)
all.cons.pred$pop<-factor(all.cons.pred$pop,levels=c("gb","wh","oy","bf","fb","sk","wp","hm"))

ggplot(all.cons.pred,aes(x=Temp,y=rate,group=pop))+geom_line(aes(group=pop,color=pop,linetype=pop),size=1.5)+
  ylab("Consumption Rate (oysters/day")+xlab("Common Garden Temperature")+theme_classic()+
  scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")

##remove pac for nsf
 nopaccons<-all.cons.pred[!all.cons.pred$oce=="p",]
 ggplot(nopaccons,aes(x=Temp,y=rate,group=pop))+geom_line(aes(group=pop,color=pop,size=pop))+
   ylab("Consumption Rate (oysters/day)")+xlab("Common Garden Temperature (°C)")+theme_classic()+
   scale_x_continuous(breaks=c(16,20,24,26,28,30))+
   scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA"),name="Population")+
   scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato"),labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA"),name="Population")+
 theme_classic()+theme(text=element_text(family="arial",size=22))
 
 uro.con.sk<-uro.con[uro.con$Population=="Skidaway",]
 ggplot(uro.con.sk,aes(x=Temp,y=rate,group=interaction(Temp)))+facet_wrap(timepoint~.)+geom_boxplot(aes(fill=Temp))
 
 ##loess
uro.con2.loe<-uro.con2
uro.con2.loe$oce<-ifelse(uro.con2.loe$Population=="Great Bay","a",ifelse(uro.con2.loe$Population=="Woods Hole","a",ifelse(uro.con2.loe$Population=="Oyster","a",ifelse(uro.con2.loe$Population=="Beaufort","a",ifelse(uro.con2.loe$Population=="Folly Beach","a",ifelse(uro.con2.loe$Population=="Skidaway","a",ifelse(uro.con2.loe$Population=="Willapa","p",ifelse(uro.con2.loe$Population=="Humboldt","p",NA)))))))) #assigning ocean

#uro.con2.loe<-uro.con2.loe%>%
  filter(oce == "a")

uro.con2.loe<-uro.con2.loe%>%
    mutate(Population = fct_relevel(Population,"Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))

#uro.con2.loe$code<-uro.con2.loe$TPC.Label
#uro.con2.loe<-merge(growth.alive$cal.length,uro.con2.loe,by="code")
#uro.con2.loe$ratio<-uro.con2.loe$rate/uro.con2.loe$
  
   ggplot(uro.con2.loe,aes(x=Temp,y=rate,group=Population,color=Population))+geom_point()+geom_smooth(method="loess",se=F,size=2)+
     geom_jitter()+theme_classic()+scale_color_discrete(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt,CA"))+
     theme(text=element_text(family="arial",size=22))+ylab("Consumption Rate (Oysters/Day)")+xlab("Common Garden Temperature (°C)")
   
 ################################################
   ##########Extract breakpoints
   
   brkpts_con<-data.frame(matrix(,nrow=8,ncol=10))
   colnames(brkpts_con)<-c("pop","brkptx","brkpty","lat","mean","s.mean","q.mean","t.mean","oce","ratio")
   
   brkpts_con$pop<-list("Willapa","Humboldt","Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway")
   
   brkpts_con[1,2]<-seg.wp$psi[[2]]
   brkpts_con[2,2]<-seg.hm$psi[[2]]
   brkpts_con[3,2]<-seg.gb$psi[[2]]
   brkpts_con[4,2]<-seg.wh$psi[[2]]
   brkpts_con[5,2]<-seg.oy$psi[[2]]
   brkpts_con[6,2]<-seg.bf$psi[[2]]
   brkpts_con[7,2]<-seg.fb$psi[[2]]
   brkpts_con[8,2]<-seg.sk$psi[[2]]
   
   brkpts_con[1,3]<-(seg.wp$psi[[2]]*coef(seg.wp)[[2]])+(coef(seg.wp)[[1]])
   brkpts_con[2,3]<-(seg.hm$psi[[2]]*coef(seg.hm)[[2]])+(coef(seg.hm)[[1]])
   brkpts_con[3,3]<-(seg.gb$psi[[2]]*coef(seg.gb)[[2]])+(coef(seg.gb)[[1]])
   brkpts_con[4,3]<-(seg.wh$psi[[2]]*coef(seg.wh)[[2]])+(coef(seg.wh)[[1]])
   brkpts_con[5,3]<-(seg.oy$psi[[2]]*coef(seg.oy)[[2]])+(coef(seg.oy)[[1]])
   brkpts_con[6,3]<-(seg.bf$psi[[2]]*coef(seg.bf)[[2]])+(coef(seg.bf)[[1]])
   brkpts_con[7,3]<-(seg.fb$psi[[2]]*coef(seg.fb)[[2]])+(coef(seg.fb)[[1]])
   brkpts_con[8,3]<-(seg.sk$psi[[2]]*coef(seg.sk)[[2]])+(coef(seg.sk)[[1]])
   
   #lat
   brkpts_con$lat<-ifelse(brkpts_con$pop=="Beaufort",34.819,ifelse(brkpts_con$pop=="Folly Beach",32.660525,
                                                           ifelse(brkpts_con$pop=="Great Bay",43.089589,ifelse(brkpts_con$pop=="Humboldt",40.849448,ifelse(brkpts_con$pop=="Oyster",
                                                                                                                                                37.288562,ifelse(brkpts_con$pop=="Tomales",38.12805,ifelse(brkpts_con$pop=="Woods Hole",41.57687,ifelse(brkpts_con$pop=="Willapa",46.5007,ifelse(brkpts_con$pop=="Skidaway",31.970
                                                                                                                                                                                                                                                                                        ,NA)))))))))
   #means
   brkpts_con[1,5]<-means[1,1]
   brkpts_con[2,5]<-means[3,1]
   brkpts_con[3,5]<-means[5,1]
   brkpts_con[4,5]<-means[8,1]
   brkpts_con[5,5]<-means[7,1]
   brkpts_con[6,5]<-means[6,1]
   brkpts_con[7,5]<-means[9,1]
   brkpts_con[8,5]<-means[4,1]
   
   #s.mean
   brkpts_con[1,6]<-s.mean[1,1]
   brkpts_con[2,6]<-s.mean[3,1]
   brkpts_con[3,6]<-s.mean[5,1]
   brkpts_con[4,6]<-s.mean[8,1]
   brkpts_con[5,6]<-s.mean[7,1]
   brkpts_con[6,6]<-s.mean[6,1]
   brkpts_con[7,6]<-s.mean[9,1]
   brkpts_con[8,6]<-s.mean[4,1]
   #q.mean
   brkpts_con[1,7]<-q[6,3]
   brkpts_con[2,7]<-q[7,3]
   brkpts_con[3,7]<-q[1,3]
   brkpts_con[4,7]<-q[2,3]
   brkpts_con[5,7]<-q[3,3]
   brkpts_con[6,7]<-q[4,3]
   brkpts_con[7,7]<-q[5,3]
   brkpts_con[8,7]<-quantile(s.gcsk$WTMP,0.75,type=1)
   #t.mean
   brkpts_con[1,8]<-q[6,4]
   brkpts_con[2,8]<-q[7,4]
   brkpts_con[3,8]<-q[1,4]
   brkpts_con[4,8]<-q[2,4]
   brkpts_con[5,8]<-q[3,4]
   brkpts_con[6,8]<-q[4,4]
   brkpts_con[7,8]<-q[5,4]
   brkpts_con[8,8]<-quantile(s.gcsk$WTMP,0.9,type=1)
   
   brkpts_con$oce<-ifelse(brkpts_con$pop=="Beaufort","a",ifelse(brkpts_con$pop=="Folly Beach","a",ifelse(brkpts_con$pop=="Great Bay","a",ifelse(brkpts_con$pop=="Humboldt","p",ifelse(brkpts_con$pop=="Oyster","a",ifelse(brkpts_con$pop=="Tomales","p",ifelse(brkpts_con$pop=="Woods Hole","a",ifelse(brkpts_con$pop=="Willapa","p",ifelse(brkpts_con$pop=="Skidaway","a",NA)))))))))
   
   mods.brk_con<-list(
     "null"=glm(brkpty~1,brkpts_con,family="gaussian"),
     "lat"=glm(brkpty~lat+oce,brkpts_con,family="gaussian"),
     "mean"=glm(brkpty~mean+oce,brkpts_con,family="gaussian"),
     "s.mean"=glm(brkpty~s.mean+oce,brkpts_con,family="gaussian"),
     "q.mean"=glm(brkpty~q.mean+oce,brkpts_con,family="gaussian"),
     "t.mean"=glm(brkpty~t.mean+oce,brkpts_con,family="gaussian"))
   
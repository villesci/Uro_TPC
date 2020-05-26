##consumption##
setwd('C:/Users/drewv/Documents/UMASS/data/')
library(tidyr)
library(dplyr)
library(segmented)
library(ggplot2)

uro.con<-read.csv('uro.consumption2.csv',header=T)
uro.con$no.consumed<-as.character(uro.con$no.consumed)
uro.con$no.consumed<-as.numeric(uro.con$no.consumed)
uro.con<-filter(uro.con,no.consumed > 0)
#View(uro.con)
summary(uro.con)
uro.con$no.consumed<-na.omit(uro.con$no.consumed)

hist(uro.con$no.consumed)


hist(uro.con$rate)


plot(uro.con$rate~uro.con$Population)
plot(uro.con$rate~uro.con$Temp)


ggplot(uro.con,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+facet_wrap(Temp~.)+geom_boxplot(aes(fill=Population))
ggplot(uro.con,aes(x=Temp,y=rate,color=Population))+geom_smooth(se=F)+geom_jitter(aes(fill=Population))+facet_wrap(timepoint~.)

uro.con.16<-filter(uro.con,Temp=="16")
uro.con.20<-filter(uro.con,Temp=="20")
uro.con.24<-filter(uro.con,Temp=="24")
uro.con.26<-filter(uro.con,Temp=="26")
uro.con.28<-filter(uro.con,Temp=="28")
uro.con.30<-filter(uro.con,Temp=="30")
ggplot(uro.con.16,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+geom_boxplot(aes(fill=Population))
ggplot(uro.con.28,aes(x=timepoint,y=rate,group=interaction(Population,timepoint)))+geom_boxplot(aes(fill=Population))


full <-glm(no.consumed ~ Population +Temp +all.consumed + tank.replicate+timepoint, uro.con, family = poisson,offset=(log(duration)))
plot(full)
hist(resid(full))

#log transformation works best! fits assumptions

library(AICcmodavg)
library(MuMIn)
models<-list("null" = glm(no.consumed~1,uro.con,family=poisson,offset=log(duration)),
             "pop"= glm(no.consumed~Population,uro.con,family=poisson,offset=log(duration)),
             "temp" = glm(no.consumed~Temp,uro.con,family=poisson,offset=log(duration)),
             "all.consumed" = glm(no.consumed~all.consumed,uro.con,family=poisson,offset=log(duration)),
             "tank.rep" = glm(no.consumed~tank.replicate,uro.con,family=poisson,offset=log(duration)),
             "full" = glm(no.consumed ~ Population + Temp + all.consumed + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P+T+C"=glm(no.consumed ~ Population + Temp + all.consumed, uro.con, family = poisson,offset=log(duration)),
             "P+T+R"=glm(no.consumed ~ Population + Temp + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P+C+R" = glm(no.consumed ~ Population + all.consumed + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "T+R+C"=glm(no.consumed ~ Temp + all.consumed + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P+T"=glm(no.consumed ~ Population + Temp, uro.con, family = poisson,offset=log(duration)),
             "P+C" = glm(no.consumed ~ Population + all.consumed, uro.con, family = poisson,offset=log(duration)),
             "P+R"=glm(no.consumed ~ Population + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "T+C"=glm(no.consumed ~ Temp + all.consumed, uro.con, family = poisson,offset=log(duration)),
             "T+R"=glm(no.consumed ~ Temp + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "C+R"=glm(no.consumed ~ all.consumed + tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P+T*C"=glm(no.consumed ~ Population + Temp * all.consumed, uro.con, family = poisson,offset=log(duration)),
             "fulli" = glm(no.consumed ~ Population *Temp * all.consumed * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P*T*C"=glm(no.consumed ~ Population * Temp * all.consumed, uro.con, family = poisson,offset=log(duration)),
             "P*T*R"=glm(no.consumed ~ Population * Temp * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P*C*R" = glm(no.consumed ~ Population * all.consumed * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "T*R*C"=glm(no.consumed ~ Temp * all.consumed * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "P*T"=glm(no.consumed ~ Population * Temp, uro.con, family = poisson,offset=log(duration)),
             "P*C" = glm(no.consumed ~ Population * all.consumed, uro.con, family = poisson,offset=log(duration)),
             "P*R"=glm(no.consumed ~ Population * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "T*C"=glm(no.consumed ~ Temp * all.consumed, uro.con, family = poisson,offset=log(duration)),
             "T*R"=glm(no.consumed ~ Temp * tank.replicate, uro.con, family = poisson,offset=log(duration)),
             "C*R"=glm(no.consumed ~ all.consumed * tank.replicate, uro.con, family = poisson,offset=log(duration)))
aictab(models)
model.sel(models)
##Population + Temperature + consumed
m1<-models$'P*T*C'
plot(m1)

hist(resid(m1))
E1<-resid(m1,type="pearson")
N<-nrow(data)
p<-length(coef(m1))
sum(E1^2) / (N-p)
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
#below, I remove every snail that ran out of food. THis removes the confounding factor of starving 
uro.con2<-subset(uro.con,TPC.Label!=(6833)&TPC.Label!=(6831)&TPC.Label!=(6711)&TPC.Label!=(6632)&TPC.Label!=(6631)&TPC.Label!=(6611)&TPC.Label!=(6433)&TPC.Label!=(6422)&TPC.Label!=(6412)&TPC.Label!=(6411)&TPC.Label!=(6233)&TPC.Label!=(6212)&TPC.Label!=(6212)&TPC.Label!=(6111)&TPC.Label!=(5831)&TPC.Label!=(5813)&TPC.Label!=(5732)&TPC.Label!=(5731)&TPC.Label!=(5723)&TPC.Label!=(5711)&TPC.Label!=(5622)&TPC.Label!=(5531)&TPC.Label!=(5333)&TPC.Label!=(5313)&TPC.Label!=(5312)&TPC.Label!=(5213)&TPC.Label!=(5121)&TPC.Label!=(5112)&TPC.Label!=(4522)&TPC.Label!=(4521)&TPC.Label!=(4433)&TPC.Label!=(4432)&TPC.Label!=(4231)&TPC.Label!=(3821)&TPC.Label!=(3712)&TPC.Label!=(3433)&TPC.Label!=(3432)&TPC.Label!=(3421)&TPC.Label!=(3413)&TPC.Label!=(2422)&TPC.Label!=(2421))

ggplot(uro.con2,aes(x=Temp,y=rate,group=Population,color=Population))+geom_point()+geom_smooth()+facet_wrap(Population~.)

uro.con2<-filter(uro.con2,all.consumed=="n")
uro.con2<-filter(uro.con2,timepoint=="3")
uro.con3<-filter(uro.con,all.consumed=="n")
uro.con3<-filter(uro.con3,timepoint=="3")


ggplot(uro.con2,aes(x=Temp,y=log_rate))+geom_jitter()+geom_smooth()+facet_wrap(Population~.)
bf<-filter(uro.con2,Population=="Beaufort")
fb<-filter(uro.con2,Population=="Folly Beach")
gb<-filter(uro.con2,Population=="Great Bay")
hm<-filter(uro.con2,Population=="Humboldt")
oy<-filter(uro.con2,Population=="Oyster")
sk<-filter(uro.con2,Population=="Skidaway")
wp<-filter(uro.con2,Population=="Willapa")
wh<-filter(uro.con2,Population=="Woods Hole")

m.wh<-glm(no.consumed~Temp,wh,family=poisson,offset=log(duration))
seg.wh<-segmented(m.wh,seg.Z = ~Temp, psi=24)

m.gb<-glm(no.consumed~Temp,gb,family=poisson,offset=log(duration))
seg.gb<-segmented(m.gb,seg.Z = ~Temp, psi=24)

m.oy<-glm(no.consumed~Temp,oy,family=poisson,offset=log(duration))
seg.oy<-segmented(m.oy,seg.Z = ~Temp, psi=24)

m.bf<-glm(no.consumed~Temp,bf,family=poisson,offset=log(duration))
seg.bf<-segmented(m.bf,seg.Z = ~Temp, psi=29,seg.control(maxit.glm=15),fix.npsi=T,n.boot=50)

m.fb<-glm(no.consumed~Temp,fb,family=poisson,offset=log(duration))
seg.fb<-segmented(m.fb,seg.Z = ~Temp, psi=24)

m.sk<-glm(no.consumed~Temp,sk,family=poisson,offset=log(duration))
seg.sk<-segmented(m.sk,seg.Z = ~Temp, psi=24)

m.hm<-glm(no.consumed~Temp,hm,family=poisson,offset=log(duration))
seg.hm<-segmented(m.hm,seg.Z = ~Temp, psi=24)

m.wp<-glm(no.consumed~Temp,wp,family=poisson,offset=log(duration))
seg.wp<-segmented(m.wp,seg.Z = ~Temp, psi=24)

xmin<-min(uro.con2$Temp,na.rm=T)
xmax<-max(uro.con2$Temp,na.rm=T)

all.length.seg<-list(seg.gb,seg.wh,seg.oy,seg.bf,seg.fb,seg.sk,seg.wp,seg.hm)


##plotting predicted values
predicted.gb<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.gb$log_rate<-predict(seg.gb,predicted.gb)
predicted.gb$pop<-"gb"
predicted.gb$oce<-"a"

predicted.wh<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.wh$log_rate<-predict(seg.wh,predicted.wh)
predicted.wh$pop<-"wh"
predicted.wh$oce<-"a"

predicted.oy<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.oy$log_rate<-predict(seg.oy,predicted.oy)
predicted.oy$pop<-"oy"
predicted.oy$oce<-"a"

predicted.bf<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.bf$log_rate<-predict(seg.bf,predicted.bf)
predicted.bf$pop<-"bf"
predicted.bf$oce<-"a"

model_mcp=list(rate_mcp~1+Temp,~0+Temp)
fitbf_wt=mcp::mcp(model_mcp,data=bf,iter=10000,cores=3,prior=list(cp_1="dunif(22,30)"))
plot(fitbf_wt, q_fit = T, q_predict = T)

predicted.fb<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.fb$log_rate<-predict(seg.fb,predicted.fb)
predicted.fb$pop<-"fb"
predicted.fb$oce<-"a"

predicted.sk<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.sk$log_rate<-predict(seg.sk,predicted.sk)
predicted.sk$pop<-"sk"
predicted.sk$oce<-"a"

predicted.wp<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.wp$log_rate<-predict(seg.wp,predicted.wp)
predicted.wp$pop<-"wp"
predicted.wp$oce<-"p"

predicted.hm<-data.frame(Temp=seq(xmin,xmax,length.out=100))
predicted.hm$log_rate<-predict(seg.hm,predicted.hm)
predicted.hm$pop<-"hm"
predicted.hm$oce<-"p"

all.cons.pred<-rbind(predicted.gb,predicted.wh,predicted.oy,predicted.bf,predicted.fb,predicted.sk,predicted.wp,predicted.hm)
all.cons.pred$pop<-factor(all.cons.pred$pop,levels=c("gb","wh","oy","bf","fb","sk","wp","hm"))
all.cons.pred$Population<-ifelse(all.cons.pred$pop=="gb","Great Bay",ifelse(all.cons.pred$pop=="wh","Woods Hole",ifelse(all.cons.pred$pop=="oy","Oyster",ifelse(all.cons.pred$pop=="bf","Beaufort",ifelse(all.cons.pred$pop=="fb","Folly Beach",ifelse(all.cons.pred$pop=="sk","Skidaway",ifelse(all.cons.pred$pop=="wp","Willapa",ifelse(all.cons.pred$pop=="hm","Humboldt",NA))))))))
all.cons.pred$Population<-as.factor(all.cons.pred$Population)
all.cons.pred$Population<-factor(all.cons.pred$Population,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))


uro.con$lograte<-log(uro.con$rate)
uro.con$Population<-factor(uro.con$Population,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))


ggplot(all.cons.pred,aes(x=Temp,y=log_rate,group=pop))+geom_line(aes(group=pop,color=pop,linetype=pop),size=1.5)+
  ylab("Log Consumption log_rate (oysters/day)")+xlab("Common Garden Temperature")+theme_classic()+
  scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+scale_x_continuous(breaks=c(16,20,24,26,28,30))

ggplot(all.cons.pred,aes(x=Temp,y=log_rate,group=Population))+geom_line(aes(group=Population,color=Population,linetype=Population),size=1.5)+
   ylab("Log Consumption log_rate (oysters/day)")+xlab("Common Garden Temperature")+theme_classic()+
   scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
   scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+scale_x_continuous(breaks=c(16,20,24,26,28,30))+
   geom_point(data=uro.con,aes(x=Temp,y=lograte,group=Population))+facet_wrap(Population~.)+theme(legend.position = "none") 

 
 ##loess
uro.con2.loe<-uro.con2
uro.con2.loe$oce<-ifelse(uro.con2.loe$Population=="Great Bay","a",ifelse(uro.con2.loe$Population=="Woods Hole","a",ifelse(uro.con2.loe$Population=="Oyster","a",ifelse(uro.con2.loe$Population=="Beaufort","a",ifelse(uro.con2.loe$Population=="Folly Beach","a",ifelse(uro.con2.loe$Population=="Skidaway","a",ifelse(uro.con2.loe$Population=="Willapa","p",ifelse(uro.con2.loe$Population=="Humboldt","p",NA)))))))) #assigning ocean

#uro.con2.loe<-uro.con2.loe%>%
  filter(oce == "a")

uro.con2.loe<-uro.con2.loe%>%
    mutate(Population = fct_relevel(Population,"Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))

#uro.con2.loe$code<-uro.con2.loe$TPC.Label
#uro.con2.loe<-merge(growth.alive$cal.length,uro.con2.loe,by="code")
#uro.con2.loe$ratio<-uro.con2.loe$log_rate/uro.con2.loe$
  
   ggplot(uro.con2.loe,aes(x=Temp,y=log_rate,group=Population,color=Population))+geom_point()+geom_smooth(method="loess",se=F,size=2)+
     geom_jitter()+theme_classic()+scale_color_discrete(name="Site",labels=c("Great Bay, NH","Woods Hole, MA","Oyster, VA","Beaufort, NC","Folly Beach, SC","Skidaway, GA","Willapa, WA","Humboldt,CA"))+
     theme(text=element_text(family="arial",size=22))+ylab("Consumption log_rate (Oysters/Day)")+xlab("Common Garden Temperature (°C)")
   
 ################################################
   ##########Extract breakpoints
   
   brkpts_con<-data.frame(matrix(,nrow=8,ncol=13))
   colnames(brkpts_con)<-c("pop","brkptx","brkpty","lat","mean","s.mean","q.mean","t.mean","oce","blank","brkptxq","brkptyq","max")
   
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
   brkpts_con[1,5]<-means[2,1]
   brkpts_con[2,5]<-means[1,1]
   brkpts_con[3,5]<-means[8,1]
   brkpts_con[4,5]<-means[7,1]
   brkpts_con[5,5]<-means[6,1]
   brkpts_con[6,5]<-means[5,1]
   brkpts_con[7,5]<-means[4,1]
   brkpts_con[8,5]<-means[3,1]
   
   #s.mean
   brkpts_con[1,6]<-s.mean[2,1]
   brkpts_con[2,6]<-s.mean[1,1]
   brkpts_con[3,6]<-s.mean[8,1]
   brkpts_con[4,6]<-s.mean[7,1]
   brkpts_con[5,6]<-s.mean[6,1]
   brkpts_con[6,6]<-s.mean[5,1]
   brkpts_con[7,6]<-s.mean[4,1]
   brkpts_con[8,6]<-s.mean[3,1]
   #q.means.
   brkpts_con[1,7]<-q[6,2]
   brkpts_con[2,7]<-q[7,2]
   brkpts_con[3,7]<-q[1,2]
   brkpts_con[4,7]<-q[2,2]
   brkpts_con[5,7]<-q[3,2]
   brkpts_con[6,7]<-q[4,2]
   brkpts_con[7,7]<-q[5,2]
   brkpts_con[8,7]<-quantile(s.gcsk$WTMP,0.75,type=1)
   #t.mean
   brkpts_con[1,8]<-q[6,3]
   brkpts_con[2,8]<-q[7,3]
   brkpts_con[3,8]<-q[1,3]
   brkpts_con[4,8]<-q[2,3]
   brkpts_con[5,8]<-q[3,3]
   brkpts_con[6,8]<-q[4,3]
   brkpts_con[7,8]<-q[5,3]
   brkpts_con[8,8]<-quantile(s.gcsk$WTMP,0.9,type=1)
   #max
   brkpts_con[1,13]<-q[6,4]
   brkpts_con[2,13]<-q[7,4]
   brkpts_con[3,13]<-q[1,4]
   brkpts_con[4,13]<-q[2,4]
   brkpts_con[5,13]<-q[3,4]
   brkpts_con[6,13]<-q[4,4]
   brkpts_con[7,13]<-q[5,4]
   brkpts_con[8,13]<-q[9,4]
   
   brkpts_con$oce<-ifelse(brkpts_con$pop=="Beaufort","a",ifelse(brkpts_con$pop=="Folly Beach","a",ifelse(brkpts_con$pop=="Great Bay","a",ifelse(brkpts_con$pop=="Humboldt","p",ifelse(brkpts_con$pop=="Oyster","a",ifelse(brkpts_con$pop=="Tomales","p",ifelse(brkpts_con$pop=="Woods Hole","a",ifelse(brkpts_con$pop=="Willapa","p",ifelse(brkpts_con$pop=="Skidaway","a",NA)))))))))
   
   ###################################3
   #y point (trait performance)
  #brkpts_con<-filter(brkpts_con,pop!="Skidaway")
   
   mods.brk_con<-list(
     "null"=glm(brkpty~1,brkpts_con,family="gaussian"),
     "lat"=glm(brkpty~lat+oce,brkpts_con,family="gaussian"),
     "mean"=glm(brkpty~mean+oce,brkpts_con,family="gaussian"),
     "s.mean"=glm(brkpty~s.mean+oce,brkpts_con,family="gaussian"),
     "*q.mean"=glm(brkpty~q.mean+oce,brkpts_con,family="gaussian"),
     "*t.mean"=glm(brkpty~t.mean+oce,brkpts_con,family="gaussian"),
     "*lat"=glm(brkpty~lat*oce,brkpts_con,family="gaussian"),
     "*mean"=glm(brkpty~mean*oce,brkpts_con,family="gaussian"),
     "*s.mean"=glm(brkpty~s.mean*oce,brkpts_con,family="gaussian"),
     "*q.mean"=glm(brkpty~q.mean*oce,brkpts_con,family="gaussian"),
     "*t.mean"=glm(brkpty~t.mean*oce,brkpts_con,family="gaussian"),
     "max"=glm(brkpty~max+oce,brkpts_con,family="gaussian"),
     "*max"=glm(brkpty~max*oce,brkpts_con,family="gaussian")
     )
   
   aictab(mods.brk_con)
   summary(glm(brkpty~mean*oce,brkpts_con,family="gaussian"))
   
   ggplot(brkpts_con,aes(x=mean,y=brkpty,color=oce,fill=oce),group=oce)+
      geom_point(size=3)+theme_classic()+geom_smooth(method="lm",se=F)+ylab(label="Rate of Consumption") 
   
   ###############################################3
   #x point (t opt)
   mods.brk_con<-list(
      "null"=glm(brkptx~1,brkpts_con,family="gaussian"),
      "lat"=glm(brkptx~lat+oce,brkpts_con,family="gaussian"),
      "mean"=glm(brkptx~mean+oce,brkpts_con,family="gaussian"),
      "s.mean"=glm(brkptx~s.mean+oce,brkpts_con,family="gaussian"),
      "q.mean"=glm(brkptx~q.mean+oce,brkpts_con,family="gaussian"),
      "t.mean"=glm(brkptx~t.mean+oce,brkpts_con,family="gaussian"),
      "*q.mean"=glm(brkptx~q.mean+oce,brkpts_con,family="gaussian"),
      "*t.mean"=glm(brkptx~t.mean+oce,brkpts_con,family="gaussian"),
      "*lat"=glm(brkptx~lat*oce,brkpts_con,family="gaussian"),
      "*mean"=glm(brkptx~mean*oce,brkpts_con,family="gaussian"),
      "*s.mean"=glm(brkptx~s.mean*oce,brkpts_con,family="gaussian"),
      "*q.mean"=glm(brkptx~q.mean*oce,brkpts_con,family="gaussian"),
      "*t.mean"=glm(brkptx~t.mean*oce,brkpts_con,family="gaussian"),
      "max"=glm(brkptx~max+oce,brkpts_con,family="gaussian"),
      "*max"=glm(brkptx~max*oce,brkpts_con,family="gaussian")
      )
   
   aictab(mods.brk_con)
   summary(glm(brkptx~s.mean+oce,brkpts_con,family="gaussian"))
   
   ggplot(brkpts_con,aes(x=s.mean,y=brkptx,color=oce,fill=oce),group=oce)+
      geom_point(size=3)+theme_classic()+geom_smooth(method="lm",se=F)+ylab(label="Topt") 

   
   #####################################################################################
   #################################################################################
   ##Quadratic
   
   quadm<-(glm(no.consumed~poly(uro.con2$Temp,2)+Population,uro.con2,family=poisson,offset=log(duration)))
   hist(resid(quadm))
   plot(quadm)
   
  
   bf<-filter(uro.con2,Population=="Beaufort")
   fb<-filter(uro.con2,Population=="Folly Beach")
   gb<-filter(uro.con2,Population=="Great Bay")
   hm<-filter(uro.con2,Population=="Humboldt")
   oy<-filter(uro.con2,Population=="Oyster")
   sk<-filter(uro.con2,Population=="Skidaway")
   wp<-filter(uro.con2,Population=="Willapa")
   wh<-filter(uro.con2,Population=="Woods Hole")
   
   xminq<-min(poly(uro.con2$Temp,2),na.rm=T)
   xmaxq<-max(poly(uro.con2$Temp,2),na.rm=T)
   
   qmgb<-(glm(no.consumed~poly(gb$Temp,2,raw=T),gb,family=poisson,offset=log(duration)))
   cfgb<-coef(qmgb)
   brkpts[3,11]<-(-cfgb[2]/(2*(cfgb[3])))
   brkpts[3,12]<-cfgb[1]+cfgb[2]*brkpts[3,11]+cfgb[3]*(brkpts[3,11]^2)
   
   qmwh<-(glm(no.consumed~poly(wh$Temp,2,raw=T),wh,family=poisson,offset=log(duration)))
   cfwh<-coef(qmwh)
   brkpts[4,11]<-(-cfwh[2]/(2*(cfwh[3])))
   brkpts[4,12]<-cfwh[1]+cfwh[2]*brkpts[4,11]+cfwh[3]*(brkpts[4,11]^2)
   
   qmoy<-(glm(no.consumed~poly(oy$Temp,2,raw=T),oy,family=poisson,offset=log(duration)))
   cfoy<-coef(qmoy)
   brkpts[5,11]<-(-cfoy[2]/(2*(cfoy[3])))
   brkpts[5,12]<-cfoy[1]+cfoy[2]*brkpts[5,11]+cfoy[3]*(brkpts[5,11]^2)
   
   qmbf<-(glm(no.consumed~poly(bf$Temp,2,raw=T),bf,family=poisson,offset=log(duration)))
   cfbf<-coef(qmbf)
   brkpts[6,11]<-(-cfbf[2]/(2*(cfbf[3])))
   brkpts[6,12]<-cfbf[1]+cfbf[2]*brkpts[6,11]+cfbf[3]*(brkpts[6,11]^2)
   
   
   qmfb<-(glm(no.consumed~poly(fb$Temp,2,raw=T),fb,family=poisson,offset=log(duration)))
   cffb<-coef(qmfb)
   brkpts[7,11]<-(-cffb[2]/(2*(cffb[3])))
   brkpts[7,12]<-cffb[1]+cffb[2]*brkpts[7,11]+cffb[3]*(brkpts[7,11]^2)
   
   qmsk<-(glm(no.consumed~poly(sk$Temp,2,raw=T),sk,family=poisson,offset=log(duration)))
   cfsk<-coef(qmsk)
   brkpts[8,11]<-(-cfsk[2]/(2*(cfsk[3])))
   brkpts[8,12]<-cfsk[1]+cfsk[2]*brkpts[8,11]+cfsk[3]*(brkpts[8,11]^2)
   
   qmwp<-(glm(no.consumed~poly(wp$Temp,2,raw=T),wp,family=poisson,offset=log(duration)))
   cfwp<-coef(qmwp)
   brkpts[1,11]<-(-cfwp[2]/(2*(cfwp[3])))
   brkpts[1,12]<-cfwp[1]+cfwp[2]*brkpts[1,11]+cfwp[3]*(brkpts[1,11]^2)
   
   qmhm<-(glm(no.consumed~poly(hm$Temp,2,raw=T),hm,family=poisson,offset=log(duration)))
   cfhm<-coef(qmhm)
   brkpts[2,11]<-(-cfhm[2]/(2*(cfhm[3])))
   brkpts[2,12]<-cfhm[1]+cfhm[2]*brkpts[2,11]+cfhm[3]*(brkpts[2,11]^2)
   
   
   uro.con2$Population<-factor(uro.con2$Population,levels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"))
   
   #poisson graph - not a good visual
  # ggplot(uro.con2,aes(y=no.consumed,x=Temp,group=Population,color=Population,linetype=Population))+
  #   scale_x_continuous(breaks=c(16,20,24,26,28,30))+
  #    scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  #    scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  #    scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
  #    theme_classic()+labs(y="Rate of Consumption",x="Temperature (°C)")+facet_wrap(Population~.)+geom_point()
   ggplot(uro.con2,aes(y=rate,x=Temp,group=Population,linetype=Population))+
      geom_smooth(method='glm',formula=y~poly(x,2),se=F,aes(color=Population,size=Population))+theme_classic()+
      scale_x_continuous(breaks=c(16,20,24,26,28,30))+
      scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      theme_classic()+labs(y="Rate of Consumption",x="Temperature (°C)")+scale_y_continuous(breaks=c(0,1,2,3),limits=c(0,3))
   
   
   
   ggplot(uro.con2,aes(y=rate,x=Temp,group=Population,linetype=Population))+
      geom_smooth(method='glm',formula=y~poly(x,2),se=F,aes(color=Population,size=Population))+theme_classic()+
      scale_x_continuous(breaks=c(16,20,24,26,28,30))+
      scale_color_manual(values=c("dark violet","navy","forest green","gold","dark orange","tomato","dark violet","navy"),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      scale_linetype_manual(values=c(1,1,1,1,1,1,3,3),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      scale_size_manual(values=c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2),labels=c("Great Bay","Woods Hole","Oyster","Beaufort","Folly Beach","Skidaway","Willapa","Humboldt"),name="Population")+
      theme_classic()+labs(y="Rate of Consumption",x="Temperature (°C)")+facet_wrap(Population~.)+geom_point()+theme(legend.position = "none")+scale_y_continuous(breaks=c(0,1,2,3),limits=c(0,3))
   
   
   mods.brkyq<-list(
      "null"=glm(brkptyq~1,brkpts_con,family="gaussian"),
      "lat"=glm(brkptyq~lat+oce,brkpts_con,family="gaussian"),
      "mean"=glm(brkptyq~mean+oce,brkpts_con,family="gaussian"),
      "s.mean"=glm(brkptyq~s.mean+oce,brkpts_con,family="gaussian"),
      "q.mean"=glm(brkptyq~q.mean+oce,brkpts_con,family="gaussian"),
      "t.mean"=glm(brkptyq~t.mean+oce,brkpts_con,family="gaussian"),
      "*lat"=glm(brkptyq~lat*oce,brkpts_con,family="gaussian"),
      "*mean"=glm(brkptyq~mean*oce,brkpts_con,family="gaussian"),
      "*s.mean"=glm(brkptyq~s.mean*oce,brkpts_con,family="gaussian"),
      "*q.mean"=glm(brkptyq~q.mean*oce,brkpts_con,family="gaussian"),
      "*t.mean"=glm(brkptyq~t.mean*oce,brkpts_con,family="gaussian"),
      "max"=glm(brkptyq~max+oce,brkpts_con_con,family="gaussian"),
      "*max"=glm(brkptyq~max*oce,brkpts_con_con,family="gaussian")
   )
   
   aictab(mods.brkyq)
   
   brkptxmq<-glm(brkptyq~mean+oce,brkpts_con,family="gaussian")
   summary(brkptxmq)
   
   ggplot(brkpts_con,aes(x=lat,y=brkptyq,color=oce),group=interaction(oce))+
      geom_point(size=3)+geom_smooth(aes(group=oce),method="lm",se=F)+theme_classic()+
      ylab("Maximal Growth (g)")+
      xlab("Latitude (degrees)")+
      theme(text=element_text(family="arial",size=22))+
      scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
      scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))
   
   mods.brkxq<-list(
      "null"=glm(brkptxq~1,brkpts_con,family="gaussian"),
      "lat"=glm(brkptxq~lat+oce,brkpts_con,family="gaussian"),
      "mean"=glm(brkptxq~mean+oce,brkpts_con,family="gaussian"),
      "s.mean"=glm(brkptxq~s.mean+oce,brkpts_con,family="gaussian"),
      "q.mean"=glm(brkptxq~q.mean+oce,brkpts_con,family="gaussian"),
      "t.mean"=glm(brkptxq~t.mean+oce,brkpts_con,family="gaussian"),
      "*lat"=glm(brkptxq~lat*oce,brkpts_con,family="gaussian"),
      "*mean"=glm(brkptxq~mean*oce,brkpts_con,family="gaussian"),
      "*s.mean"=glm(brkptxq~s.mean*oce,brkpts_con,family="gaussian"),
      "*q.mean"=glm(brkptxq~q.mean*oce,brkpts_con,family="gaussian"),
      "*t.mean"=glm(brkptxq~t.mean*oce,brkpts_con,family="gaussian"),
      "max"=glm(brkptxq~max+oce,brkpts_con_con,family="gaussian"),
      "*max"=glm(brkptxq~max*oce,brkpts_con_con,family="gaussian")
   )
   
   aictab(mods.brkxq)
   
   brkptxmq<-glm(brkptx~lat+oce,brkpts_con,family="gaussian")
   summary(brkptxmq)
   
   ggplot(brkpts_con,aes(x=lat,y=brkptxq,color=oce),group=interaction(oce))+
      geom_point(size=3)+geom_smooth(aes(group=oce),method="lm",se=F)+theme_classic()+
      ylab("Thermal Optima")+
      xlab("Latitude (degrees)")+
      theme(text=element_text(family="arial",size=22))+
      scale_color_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))+
      scale_fill_manual(labels=c("Atlantic","Pacific"),name="Ocean", values=c('#F8766D','#00BFC4'))
   
   

   
   brkpts_con$pop<-unlist(brkpts_con$pop)
   write.csv(brkpts_con,"C:/Users/drewv/Documents/UMASS/data/consume.csv",row.names=F)

   
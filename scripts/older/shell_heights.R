##This short script answers the question are P1 snails from our different colletions sites different?


library(here)
here()
all<-read.csv(here::here("data/all.shell.size.csv"))
library(ggplot2)
all$Collection.site<-factor(all$Collection.site,levels=c("SK","FB","BF","OY","WH","GB","TO","WP",ordered=T))
all<-na.omit(all)
ggplot(all,aes(x=Collection.site,y=Shell.height,group=Collection.site))+geom_boxplot()
a<-aov(Shell.height~Collection.site,data=all)
summary(a)       
TukeyHSD(a)

#The answer is yes. There are signficant differences between sites. 
##This short script, to be incorporated into the growth markdown, answers the question do initial
#snail sizes differ between populations? If they do, we will have to not use raw final snail sizes

library(ggplot2)
library(here)
growth.test<-read.csv(here::here("data/iniital.snail.growth.test.csv"))
growth.test<-na.omit(growth.test)
growth.test$lat<-as.factor(growth.test$lat)

growth.test$lat<-ifelse(growth.test$Population=="Beaufort",34.819,ifelse(growth.test$Population=="Folly Beach",32.660525,ifelse(growth.test$Population=="Great Bay",43.089589,ifelse(growth.test$Population=="Humboldt",40.849448,ifelse(growth.test$Population=="Oyster",37.288562,ifelse(growth.test$Population=="Tomales",38.12805,ifelse(growth.test$Population=="Woods Hole",41.57687,ifelse(growth.test$Population=="Willapa",46.501,ifelse(growth.test$Population=="Skidaway",31.989781,NA)))))))))
#growth.test$lat<-as.factor(growth.test$lat)

ggplot(growth.test,aes(x=lat,y=length..mm.,group=lat))+geom_boxplot()+geom_abline(intercept=1.7135,slope = -0.004744)+geom_smooth()
growth.test$length..mm.<-as.numeric(growth.test$length..mm.)
r<-aov(length..mm.~Population,data=growth.test)
summary(r)

res<-TukeyHSD(r,which="Population",ordered=T)
##looks like there is a significant diference in shell length. So once this is all completed, we might do growth from initial instead of total growth. 
options(scipen = 999)
table<-res[["Population"]]
table[order(table[,'p adj']),]

#initial snail sizes do differ between sites. Therefore, we must standardize shell length by
#subtracting initial size from final size. 

summary(glm(length..mm.~lat,growth.test,family="gaussian"))

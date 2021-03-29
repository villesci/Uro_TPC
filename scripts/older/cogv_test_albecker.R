#######################################################################################
##                      Co/counter gradient Analysis                                 ##
##  Authors: Molly Albecker, Thais Bittar, Geoff Trussell, Katie Lotterhos           ##
#######################################################################################

# Load packages
library("emmeans")
library("lme4")
library("dplyr")
library("gridExtra")

# 1. Load Data
cogv <- read.csv("data/growth.csv")

# 2. Load Functions (CovarianceDataFunctions.R)
setwd("~/Documents/GitHub/CnGV/CnGV/src/")
source("CovarianceDataFunctions.R")

# 3. Format Data 
# Need: "gen_factor" column -as factor (each genotype/population = "G_1", "G_2", etc.)
# Need: "exp_env_factor" column - as factor (each experimental treatment = "E_1", "E_2", etc.)
# Need: "nat_env_factor" column - as factor (each genotype's native environment = "E_1", "E_2", etc.)
# Need: "phen_data" column - simply the column with focal phenotypic data
data.df<-data.frame(matrix(ncol=3,nrow=432))
colnames(data.df)<-c('gen_factor','exp_env_factor','nat_env_factor')

data.df$gen_factor <- factor(cogv$pop)

data.df$nat_env_factor <- ifelse(data.df$gen_factor=="Beaufort",20.34943957
,ifelse(data.df$gen_factor=="Folly Beach",19.737689
,ifelse(data.df$gen_factor=="Great Bay",14.4331229
,ifelse(data.df$gen_factor=="Humboldt",12.40097379
,ifelse(data.df$gen_factor=="Oyster",21.45739396
,ifelse(data.df$gen_factor=="Woods Hole",13.91728209
,ifelse(data.df$gen_factor=="Willapa",12.1004426
,ifelse(data.df$gen_factor=="Skidaway",17.08423552
,NA))))))))

data.df$exp_env_factor<-as.factor(cogv$temp)

data.df$nat_env_factor<-as.factor(data.df$nat_env_factor)

data.df$phen_data <- as.numeric(cogv$cal.length)

data.df<-na.omit(data.df)
# 4. Establish starting conditions
n_boot <- 99
  
# 5. Is the data "raw" or "means" format? 
data_type = "raw"

# 6. Standardize by standard deviation of group means
if(data_type == "raw"){ 
  data.df$group = paste(data.df$gen_factor,data.df$exp_env_factor,sep = "-")
  data.df$phen_corrected = (data.df$phen_data - mean(data.df$phen_data))/sd(tapply(data.df$phen_data, data.df$group, mean))
  }else{
  data.df$avg_phen_corrected = (data.df$phen_data - mean(data.df$phen_data))/sd(data.df$phen_data)
  }
  
# 7. Sanity Plots to form expectations
 a = ggplot(data.df,aes(x=exp_env_factor,y=phen_corrected, group = gen_factor, colour=nat_env_factor))+
   geom_point()+geom_smooth(method="glm")+theme_classic() + ggtitle("Standardized Data") 
 b = ggplot(data.df,aes(x=exp_env_factor,y=phen_data, group = gen_factor, colour=nat_env_factor))+
   geom_point()+geom_smooth(method="glm")+theme_classic() + ggtitle("Raw Data")
 grid.arrange(a,b)
 
# 8. Run the analysis
output = amarillo_armadillo(data.df, n_boot, data_type)
output


install.packages("survival")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("haven")

library(survival)
library(lattice)
library(Hmisc)
library(haven)

rm(list=ls())
dmef <- read_sas("C:/Users/Rohan/Desktop/UIC/Fall 23/IDS 462/Week 3/dmefzip.sas7bdat")

QOnly = subset(dmef, select = -c(ADICODE,BRANCHCD,COUNTYCD,COUNTYNM,DMACODE,FILETYPE,    
MSA,MULTZIP,NIELCODE,POFFNAME,RECTYPE,STATEABR,STATECD))
nrow(QOnly)
ncol(QOnly)
NoMiss<-dmef[complete.cases(QOnly),]
nrow(NoMiss)
str(NoMiss)
sum(is.na(QOnly))
is.na(QOnly)

missing_values <- colSums(is.na(QOnly))

# Print the column names and the number of missing values
for (col in names(missing_values)) {
  message(col, ": ", missing_values[col])
}




34297-33377
920*33
30360-1650

library(dplyr)

install.packages("tidyr")
library(tidyr)
ForFactor<-QOnly %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))

Standard<-ForFactor %>% mutate(across(where(is.numeric), scale))

install.packages("psych")
library(psych)
Factors <- principal(Standard[,2:33], nfactors=5, rotate="varimax")
Factors

library(haven)
write_sav(Standard, "Standard.sav")




perform_kmeans<- function(data, num_clusters, seed){
  set.seed(seed)
  kmeans(data, centers= num_clusters, nstart=1)
}
results<-list()
cluster_sizes<-c(5,6,7)
seeds<-c(1,2,3,4)
for(k in cluster_sizes){
  for(seed in seeds){
    result_name<-paste("k", k, "seed", seed, sep="_")
    results[[result_name]]<-perform_kmeans(mydata, num_clusters=k, seed= seed)
  }
}


cart_model<-rpart(target~.,data= training_data , method="class")
scoring_data<- scoring_data %>% mutate(predicted= predict(cart_model, newdata= scoring_data, type="class"))
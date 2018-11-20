library(igraph)
library(data.table)
library(network)
library(tidyverse)
library(readxl)
library(tidyr)
library(splitstackshape)
library(magrittr)
library("zoo")
library(ggplot2)
library(lubridate)
library(dplyr)
library('combinat')
library(plm)
library(pglm)
library(scatterplot3d)
library(rgl)
library(nnet)
setwd("C:/Users/Jacky Yang/Desktop/Social Network/hw5")
firm<-fread("investor_firms.csv",header=TRUE)
deals<-fread("investors_and_deals.csv",header=TRUE)
startup_com<-fread("startup_companies.csv",header=TRUE)
startup_deal<-fread("startups_and_deals.csv",header=TRUE)
all<-fread("all.csv",header = TRUE)
combn<-fread("combn.csv",header = TRUE)

#create edgelist of investors
deals<-as.data.table(deals)
deals[, min_two := .N > 1, by =`Deal_Id`]
deals_d = deals[min_two==TRUE]
combn<-deals_d[,as.data.table(t(combn(Investor_Id,2))), by=`Deal_Id`]
combn_opp<-as.data.table(cbind(combn$Deal_Id,combn$V2,combn$V1))
colnames(combn_opp)<-c("Deal_Id","V1","V2")
combn2<-rbind(combn,combn_opp)

#join all data tables together
merge1<-left_join(combn2,deals,by=c("V1"="Investor_Id","Deal_Id"="Deal_Id"))
merge2<-left_join(merge1,startup_deal,by=c("Deal_Id"="DealId"))
merge3<-left_join(merge2,startup_com,by=c("CompanyId"="CompanyID"))
merge4<-left_join(merge3,firm,by=c("V1"="InvestorId"))


# question 1a -------------------------------------------------------------
all<-as.data.table(merge4)
#assume NAs are missing data so I got rid of them
all<-all[!is.na(all$successful_investments)]
#got rid of the outlier
all<-all[-which(all$successful_investments==max(all$successful_investments,na.rm=TRUE)),]
#count the number of industry
all[, count_industry :=  length(unique(Primary_Industry_Code)), by = "V1"]
#count the total number of industry
all[, count_total_invest :=  length(unique(CompanyId)), by = "V1"]
#calculate the diverse
all[, diverse := count_industry/count_total_invest,by="V1"]


plot(all$diverse,all$successful_investments)

#write.csv(all,"all.csv")
# question 1b -------------------------------------------------------------
lm1<-lm(successful_investments~diverse,data=all)

summary(lm1)
# question 2a -----------------------------------------------------------------------

#write.csv(combn,"combn.csv")

#backup code
# combn<-combn[,2:4]
# combn_m<-left_join(combn2,all, by = c("Deal_Id"="Deal_Id","V1"="InvestorId"))
# combn_m<-as.data.table(combn_m)

#count how many deals are there
all[,count_deal := length(unique(Deal_Id)), by = list(`V1`,`V2`)]
#count how many deals are in lead by "V1" investor
all[,count_lead := sum(Lead_Investor),by = list(`V1`,`V2`)]
#calculate the status
all[,status := count_lead/count_deal]
#select only relevant columns as edgelist and status as weight
edgelist<-select(all,"V1","V2","status")
#create graph object from the edgelist
g<-graph.data.frame(edgelist,directed = TRUE)
#add status as weight
g<-set_edge_attr(g,"weight",value = edgelist$status)
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id=V(g)$name
name<-as.data.table(list(Investor=id,status = eigen))
#only get relevant columns
short<-select(all,"V1","successful_investments")
#new edgelist
new_edge<-left_join(name,short,by=c("Investor"="V1"))


plot(new_edge$status,new_edge$successful_investments)

# question 2b -------------------------------------------------------------
#as status is a significant coefficient, status is related to the number of successful investments
lm2<-lm(successful_investments~status.V1,data=new_edge)
summary(lm2)


# question 3a -------------------------------------------------------------
#as the interaction is a significant coefficient, status is related to the number of successful investments
lm3<-lm(successful_investments~status*diverse,data=all)
summary(lm3)


# question 3b -------------------------------------------------------------

# set up scaled grid of (x,y) values
diverse = seq(0,1000, by=20)
status = seq(0,1000, by=20)
values = expand.grid(diverse=diverse, status=status)
# prediction from the model
values$successful_investments = predict(lm3,newdata=values)
# regular 3d plot
scatterplot3d(values$diverse, values$status, values$successful_investments)
# interactive 3d plot you can move around
plot3d(values$diverse, values$status, values$successful_investments)


# question 4 -------------------------------------------------------------
#all predictors are significant.Specifically, diversification is the most significant predictor, which means it's the most beneficial to be diversified for startup firms.
#this means diversification and status are both very important for startup firms when they are in the stages of Generating Revenue, Profitable and Rampup.

#join the comprehensive table with startup_com table
joined2<-left_join(all,startup_com, by = c("CompanyId"="CompanyID"))
joined2<-as.data.table(joined2)
#classify the business status accordingly
joined2[,b_status := ifelse(Business_Status.y=="Startup" |grepl("Clinical Trials", Business_Status.y)|grepl("Product",Business_Status.y),"Rampup",
                            ifelse(grepl("Generating Revenue",Business_Status.y),"Generating_rev",
                                   ifelse(Business_Status.y=="Profitable","Profitable",
                                          ifelse(grepl("Bankruptcy",Business_Status.y)|Business_Status.y=="Out of Business","Failed","other"))))]
#omit NAs 
joined2_short<-na.omit(select(joined2,"status","diverse","b_status"))
#only select those 4 stages
joined2_short<-subset(joined2_short,b_status=="Rampup"|b_status=="Profitable"|b_status=="Generating_rev"|b_status=="Failed")

model = multinom(b_status~ diverse + status + diverse*status,data=joined2_short)
    
z = summary(model)$coefficients/summary(model)$standard.errors

(1 - pnorm(abs(z), 0, 1)) * 2






















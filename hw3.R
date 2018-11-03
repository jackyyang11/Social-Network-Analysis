setwd("C:/Users/Jacky Yang/Desktop/Social Network/hw3")

district<-read.csv("district_information.csv")
border<-read.csv("border_information.csv")
rain<-read.csv("rain_information.csv")
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
# question 1a new -------------------------------------------------------------
rain_nona<-rain[!(is.na(rain$spi) | rain$spi==""), ]
district_short<-district[,1:4]

unique_district<-unique(district$district)
unique<-sort(unique(district$year))
party_yearlist<-list()
district_list<-list()
avg_rainlist<-list()
newparty_list<-list()
state_list<-list()

bydistrict<-split(district_short,district_short$district)
for (disno in 1:length(bydistrict)){
  dis<-names(bydistrict[disno])
  district_d<-as.data.frame(bydistrict[disno])
  colnames(district_d)<-c("state","district","year","new_parties")
  for (uni in 1:length(unique)){
    disuni_year<-unique[uni]
    #overlap
    if (uni>1){
      #previous year
      pre_year<-unique[uni-1]
      if ((dis %in% rain_nona$district) & (disuni_year %in% district_d$year)){
        #rain data with cutoff years and all district
        just_year<-subset(rain_nona,year<=disuni_year)
        #rain data with cutoff years and just one district
        dis_year<-subset(just_year,district==dis)
        #select rain data by cutoff year
        rain_year<-subset(dis_year,year<=disuni_year)
        #select rain data by previous cutoff year
        rain_preyear<-subset(dis_year,year<=pre_year)
        #only include year between previous and current cutoff year
        nooverlap<-subset(rain_year, !(year %in% rain_preyear$year))
        #calculate avg spi
        avg_rain<-sum(nooverlap$spi)/nrow(nooverlap)
        if (!is.na(avg_rain)){
          party_yearlist<-c(party_yearlist,disuni_year)
          avg_rainlist<-c(avg_rainlist,avg_rain)
          district_list<-c(district_list,as.character(dis))
          newparty<-subset(district_d,year==disuni_year)
          newparty_list<-c(newparty_list,newparty[,4])
          state_list<-c(state_list,as.character(newparty[,1]))
        }
      }
    }
      #no overlap
      else{
        if ((dis %in% rain_nona$district) & (disuni_year %in% district_d$year)){
          #rain data with cutoff years and all district
          just_year<-subset(rain_nona,year<=disuni_year)
          #rain data with cutoff years and just one district
          dis_year<-subset(just_year,district==dis)
          #calculate avg spi
          avg_rain<-sum(dis_year$spi)/nrow(dis_year)
          if (!is.na(avg_rain)) {
            party_yearlist<-c(party_yearlist,disuni_year)
            avg_rainlist<-c(avg_rainlist,avg_rain)
            district_list<-c(district_list,as.character(dis))
            newparty<-subset(district_d,year==disuni_year)
            newparty_list<-c(newparty_list,newparty[,4])
            state_list<-c(state_list,as.character(newparty[,1]))
          }
        }
      }
  }
}
avg_d<-data.frame(unlist(avg_rainlist))
no_naavg<-data.frame(avg_d[complete.cases(avg_d), ])

party_d<-data.frame(unlist(party_yearlist))
no_naparty<-data.frame(party_d[complete.cases(party_d), ])

district_d<-data.frame(unlist(district_list))
no_nadistrict<-data.frame(district_d[complete.cases(district_d), ])

newparty_d<-data.frame(unlist(newparty_list))
no_nanewparty<-data.frame(newparty_d[complete.cases(newparty_d), ])

state_d<-data.frame(unlist(state_list))
no_nastate<-data.frame(state_d[complete.cases(state_d), ])

bind<-data.frame(cbind(no_naavg,no_naparty,no_nadistrict,no_nanewparty,no_nastate))
graph_d <- as.data.frame(lapply(bind, unlist))
graph_d<-graph_d[order(graph_d$party_d.complete.cases.party_d....),]
colnames(graph_d)<-c("Avg_SPI","Year","District","New_Founded_Party","State")

ggplot(data=graph_d,aes(x=Year,y=Avg_SPI))+geom_point()+ylab("Avg SPI")+xlab("Year")
ggplot(data=graph_d,aes(x=Year,y=New_Founded_Party))+geom_point()+ylab("New Founded Party")+xlab("Year")
ggplot(data=graph_d,aes(x=New_Founded_Party,y=Avg_SPI))+geom_point()+ylab("Avg SPI")+xlab("New Founded Party")      
  





# question 1b -------------------------------------------------------------

unique(district$year)
neighbor<-aggregate(Avg_SPI~State+Year,data=graph_d,FUN=mean)




# question 1c -------------------------------------------------------------

party_yearlist<-list()
district_list<-list()
avg_rainlist<-list()
newparty_list<-list()
state_list<-list()
flood_list<-list()
drought_list<-list()
for (year in 1:length(district$year)){
  #year in district dataset
  party_year<-district[year,3]
  #district in district dataset
  dis<-as.character(district[year,2])
  #previous cutoff year in district dataset
  pre_year<-district[year-1,3]
  if (dis %in% rain_nona$district){
    #rain data with cutoff years and all district
    just_year<-subset(rain_nona,year<=party_year)
    #rain data with cutoff years and just one district
    dis_year<-subset(just_year,district==dis)
    #if no overlap situation exists
    if (party_year==min(dis_year$year)){
      #select rain data by cutoff year
      rain_year<-subset(dis_year,year<=party_year)
      flood<-0
      drought<-0
      if (nrow(rain_year>0)){
      for (rainyear in 1:nrow(rain_year)){
        if (rain_year[rainyear,4]>0){
          flood<-flood+1
        }else if (rain_year[rainyear,4]<0){
          drought<-drought+1
        }
      }
      }
      if (nrow(rain_year)>0){
        party_yearlist<-c(party_yearlist,party_year)
        avg_rainlist<-c(avg_rainlist,avg_rain)
        district_list<-c(district_list,as.character(district[year,2]))
        newparty_list<-c(newparty_list,district[year,4])
        state_list<-c(state_list,as.character(district[year,1]))
        flood_list<-c(flood_list,flood)
        drought_list<-c(drought_list,drought)
      }
    }
    else {
      #select rain data by cutoff year
      rain_year<-subset(dis_year,year<=party_year)
      #select rain data by previous cutoff year
      rain_preyear<-subset(dis_year,year<=pre_year)
      #only include year between previous and current cutoff year
      nooverlap<-subset(rain_year, !(year %in% rain_preyear$year))
      flood<-0
      drought<-0
      if (nrow(nooverlap)>0){
        for (rainyear in 1:nrow(nooverlap)){
          if (nooverlap[rainyear,4]>0){
            flood<-flood+1
          }else if (nooverlap[rainyear,4]<0){
            drought<-drought+1
          }
        }
      }
      
      if (nrow(nooverlap)>0){
        party_yearlist<-c(party_yearlist,party_year)
        avg_rainlist<-c(avg_rainlist,avg_rain)
        district_list<-c(district_list,as.character(district[year,2]))
        newparty_list<-c(newparty_list,district[year,4])
        state_list<-c(state_list,as.character(district[year,1]))
        flood_list<-c(flood_list,flood)
        drought_list<-c(drought_list,drought)
      }
    }
  }else{next}
}

avg_d<-data.frame(unlist(avg_rainlist))
no_naavg<-data.frame(avg_d[complete.cases(avg_d), ])

party_d<-data.frame(unlist(party_yearlist))
no_naparty<-data.frame(party_d[complete.cases(party_d), ])

district_d<-data.frame(unlist(district_list))
no_nadistrict<-data.frame(district_d[complete.cases(district_d), ])

newparty_d<-data.frame(unlist(newparty_list))
no_nanewparty<-data.frame(newparty_d[complete.cases(newparty_d), ])

state_d<-data.frame(unlist(state_list))
no_nastate<-data.frame(state_d[complete.cases(state_d), ])

flood_d<-data.frame(unlist(flood_list))
no_naflood<-data.frame(flood_d[complete.cases(flood_d), ])

drought_d<-data.frame(unlist(drought_list))
no_nadrought<-data.frame(drought_d[complete.cases(drought_d), ])

bind2<-data.frame(cbind(no_naparty,no_nadistrict,no_nanewparty,no_nastate,no_naflood,no_nadrought))
graph_d2 <- as.data.frame(lapply(bind2, unlist))
graph_d2<-graph_d2[order(graph_d2$party_d.complete.cases.party_d....),]
colnames(graph_d2)<-c("Year","District","New_Founded_Party","State","Flood Year","Drought Year")
# question 2a -------------------------------------------------------------





























  
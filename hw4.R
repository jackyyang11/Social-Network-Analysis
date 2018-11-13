setwd("C:/Users/Jacky Yang/Desktop/Social Network/hw4")

library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(MASS)
library(proxy)
library(igraph)

producers<-read.csv("producers_and_films.csv")
keyword<-read.csv("film_keywords.csv")
box<-read.csv("box_office_revenues.csv")
subsidiary<-read.csv("production_subsidiaries.csv")

options(warn = -1)

# data cleaning -----------------------------------------------------------
producers_us2<-producers[producers$country=='us',]
producers_us2<-as.data.table(producers_us2)
producers_us2[, count_prod :=  .N, by = c("year", "pcindex")]
producers_us2[, producer := ifelse(count_prod > 1, "generalist", "specialist")]
producers_us2[, single_co := ifelse(.N > 1, "co", "solo"), by = "pindex"]
producers_us2[, specific_type := ifelse(single_co == "solo", 
                                       ifelse(producer=='specialist', "Peripheral solo", "Central solo"),
                                       ifelse(length(unique(producer))>1, "Hybrid co", ifelse(unique(producer)=='specialist',"Peripheral co", "Central co"))), by = "pindex"]
producers_us<-producers[producers$country=='us',]
box$return<-box$total_box/box$release_coverage
join_box<-left_join(producers_us,box,by="pindex")
#merge keyword with film table
producers_us_short<-producers_us[,1:2]
join_key<-unique(left_join(unique(keyword),unique(producers_us_short),by="pindex"))
# question 1a -------------------------------------------------------------
producers_us<-as.data.table(producers_us)
producers_us[, count_prod :=  .N, by = c("year", "pcindex")]
producers_us[, producer := ifelse(count_prod > 1, "generalist", "specialist")]
producers_us[, single_co := ifelse(.N > 1, "co", "solo"), by = "pindex"]
producers_us[, specific_type := ifelse(single_co == "solo", 
                            ifelse(producer=='specialist', "Peripheral solo", "Central solo"),
                            ifelse(length(unique(producer))>1, "Hybrid co", ifelse(unique(producer)=='specialist',"Peripheral co", "Central co"))), by = "pindex"]
producers_us[,op_y:=year-min(year),by="pcindex"]
producers_us[, count_central_co := length(specific_type=="Central co"), by = c("year", "pcindex")]
producers_us[, count_peripheral_co := length(specific_type=="Peripheral co"), by = c("pcindex","year")]
producers_us[, count_central_solo := length(specific_type=="Central solo"), by = c("year", "pcindex")]
producers_us[, count_peripheral_solo := length(specific_type=="Peripheral solo"), by = c("pcindex","year")]
producers_us[, count_hybrid := length(specific_type=="Hybrid co"), by = c("year", "pcindex")]
producers_us[, total_film := length(project), by = c("year", "pindex")]

producers_us<-left_join(producers_us,box,by="pindex")
producers_us<-setDT(left_join(producers_us,subsidiary,by="pcindex"))
producers_us[, subsidiary := ifelse(!is.na(first_year),ifelse((year>first_year) & (year<last_year),1L,0),0)]
#I submitted this file for your reference
write.csv(producers_us,"producers_us.csv")

#determine if the keyword is new,comment out to save run time each run and just read the output from the first run
# for (year in 1:nrow(join_key)){
#   pre<-join_key[year,4]-3
#   current<-join_key[year,4]
#   join_key<-as.data.frame(join_key)
#   only3<-subset(join_key,year>pre & year<current)
#   if (!join_key[year,3] %in% only3$keyword_index){
#     join_key[year,5]<-"new"
#   }
#   else{
#     join_key[year,5]<-"old"
#   }
# }
#I submitted this file for your reference
# write.csv(join_key,'join_key.csv')

join_key<-read.csv("join_key.csv")
#select only relevant columns
join_key_d<-as.data.table(join_key)
join_key_d[,new_key := length(V5=="new"),by=c("pindex","year")]
join_key_d_short<-cbind(join_key_d[,2],join_key_d[,5],join_key_d[,7])
#join keyword table with the comprehensive producer table
producers_us<-left_join(unique(producers_us),unique(join_key_d_short),by=c("pindex"="pindex","year"="year"))
producers_us[is.na(producers_us)]<-0
#divide the tables into solo and co types of films
co<-subset(producers_us,specific_type=="Peripheral co" | specific_type=="Hybrid co" |specific_type=="Central co")
solo<-subset(producers_us,specific_type=="Peripheral solo"|specific_type=="Central solo")

#select only central solo
central_solo<-subset(solo,specific_type=="Central solo")
#split by year
byyear<-split(central_solo,central_solo$year)
#save new keywords by film
all_film<-data.frame()

first<-as.data.frame(byyear[1])
first_n<-nrow(first)

count_combn<-list()
count_list<-list(1194)
year_list<-list(1985)
store<-data.frame()
film_k<-data.frame()
colnames(first)
for (year in 1:33){
  d<-as.data.frame(byyear[year])
  colnames(d)<-c("pindex","year","project","pcindex","prod_company","country","count_prod","producer","single_co",
                     "specific_type","op_y","count_central_co","count_peripheral_co","count_central_solo","count_peripheral_solo",
                     "count_hybrid","total_film","total_box","budget","release_coverage","return","first_year","last_year",
                     "subsidiary","new_key")
  #this year
  this_y<-d$year
  #save this year's data into a comprehensive dataframe
  store<-rbind(store,d)
  if (year>3){
    #previous year
    pre_y<-(d$year-3)[1]
    only3<-subset(store,year>pre_y & year<this_y)
    #old keywords
    old<-na.omit(d[(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ="pindex"]
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      #new combinations
      combn<-old[,as.data.table(t(combn(keyword_index,2))), by=list(`pindex`,`year`)]
      store_combn<-c(store_combn,combn)
      only3_combn<-subset(store_combn,year>pre_y & year<this_y)
    }else{next}
    #new combinations
    new_combn<-na.omit(d[!(na.omit(combn)) %in% (na.omit(only3_combn)),])
    #new keywords
    new<-nrow(d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    film<-d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),]
    count_film<-as.data.frame((count(film$pindex)))
    film_k<-rbind(film_k,count_film)
    
    #get the year
    thatyear<-d[year,2]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
  else{
    #new keywords
    new<-length(unique(na.omit(d$keyword_index)))
    count_film<-as.data.frame((count(d$pindex)))
    film_k<-rbind(film_k,count_film)
    #new combinations
    old<-na.omit(d[unique(d$keyword_index),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ='pindex']
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), .(`pindex`)]
    }
    #get the year
    thatyear<-d[year,2]
    
    count_combn<-c(count_combn,nrow(combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
}
count_d<-data.frame(unlist(count_list))
year_d<-data.frame(unlist(year_list))
combn_d<-data.frame(unlist(count_combn))

bind<-data.frame(cbind(year_d,count_d,combn_d))
colnames(bind)<-c("year","number of new keywords","number of new combinations")
bind<-rbind(bind[1,],bind[3:34,])
g1<-ggplot(data=bind,aes(x=year,y=`number of new keywords`))+geom_line()+ggtitle("Single Generalist")
g1

#select only peripheral solo
peripheral_solo<-subset(producers_us2,specific_type=="Peripheral solo")
#split by year
byyear<-split(peripheral_solo,peripheral_solo$year)
#first year
first<-as.data.frame(byyear[1])
first_n<-nrow(first)

count_combn<-list()
count_list<-list(1627)
year_list<-list(1985)
store<-data.frame()
film_k<-data.frame()
colnames(first)
for (year in 1:33){
  d<-as.data.frame(byyear[year])
  
  colnames(d)<-c("pindex","keyword","keyword_index","year","specific_type")
  #this year
  this_y<-d$year
  #save this year's data into a comprehensive dataframe
  store<-rbind(store,d)
  if (year>3){
    #previous year
    pre_y<-(d$year-3)[1]
    only3<-subset(store,year>pre_y & year<this_y)
    #old keywords
    old<-na.omit(d[(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ="pindex"]
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), by=list(`pindex`,`year`)]
      store_combn<-c(store_combn,combn)
      only3_combn<-subset(store_combn,year>pre_y & year<this_y)
    }
    #new combinations
    new_combn<-na.omit(d[!(na.omit(combn)) %in% (na.omit(only3_combn)),])
    #new keywords
    new<-nrow(d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    film<-d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),]
    count_film<-as.data.frame((count(film$pindex)))
    film_k<-rbind(film_k,count_film)
    
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
  else{
    #new keywords
    new<-length(unique(na.omit(d$keyword_index)))
    count_film<-as.data.frame((count(d$pindex)))
    film_k<-rbind(film_k,count_film)
    #new combinations
    old<-na.omit(d[unique(d$keyword_index),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ='pindex']
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), .(`pindex`)]
    }
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
}
count_d<-data.frame(unlist(count_list))
year_d<-data.frame(unlist(year_list))
combn_d<-data.frame(unlist(count_combn))

#aggregate keywords by film
colnames(film_k)<-c("pindex","freq")
film_k<-as.data.table(film_k)
#save to the comprehensive new keywords by film table
all_film<-rbind(all_film,film_k)

bind<-data.frame(cbind(year_d,count_d,combn_d))
colnames(bind)<-c("year","number of new keywords","number of new combinations")
bind<-rbind(bind[1,],bind[3:34,])
g2<-ggplot(data=bind,aes(x=year,y=`number of new keywords`))+geom_line()+ggtitle("Single Specialist")

#select only group_spec/peripheral co
peripheral_co<-subset(co,specific_type=="Peripheral co")
#split by year
byyear<-split(group_spe_m,group_spe_m$year)
#first year
first<-as.data.frame(byyear[1])
first_n<-nrow(first)

count_combn<-list()
count_list<-list(892)
year_list<-list(1985)
store<-data.frame()
film_k<-data.frame()
colnames(first)
for (year in 1:33){
  d<-as.data.frame(byyear[year])
  
  colnames(d)<-c("pindex","keyword","keyword_index","year","specific_type")
  #this year
  this_y<-d$year
  #save this year's data into a comprehensive dataframe
  store<-rbind(store,d)
  if (year>3){
    #previous year
    pre_y<-(d$year-3)[1]
    only3<-subset(store,year>pre_y & year<this_y)
    #old keywords
    old<-na.omit(d[(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ="pindex"]
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), by=list(`pindex`,`year`)]
      store_combn<-c(store_combn,combn)
      only3_combn<-subset(store_combn,year>pre_y & year<this_y)
    }
    #new combinations
    new_combn<-na.omit(d[!(unique(na.omit(combn))) %in% (unique(na.omit(only3_combn))),])
    #new keywords
    new<-nrow(d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    film<-d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),]
    count_film<-as.data.frame((count(film$pindex)))
    film_k<-rbind(film_k,count_film)
    
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
  else{
    #new keywords
    new<-length(unique(na.omit(d$keyword_index)))
    count_film<-as.data.frame((count(d$pindex)))
    film_k<-rbind(film_k,count_film)
    #new combinations
    old<-na.omit(d[unique(d$keyword_index),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ='pindex']
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), .(`pindex`)]
    }
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
}
count_d<-data.frame(unlist(count_list))
year_d<-data.frame(unlist(year_list))
combn_d<-data.frame(unlist(count_combn))

#aggregate keywords by film
colnames(film_k)<-c("pindex","freq")
film_k<-as.data.table(film_k)
#save to the comprehensive new keywords by film table
all_film<-rbind(all_film,film_k)

bind<-data.frame(cbind(year_d,count_d,combn_d))
colnames(bind)<-c("year","number of new keywords","number of new combinations")
bind<-rbind(bind[1,],bind[3:34,])
g3<-ggplot(data=bind,aes(x=year,y=`number of new keywords`))+geom_line()+ggtitle("Group Specialist")


# group gen/central co ----------------------------------------------------

#select only central co/group_gen
central_co<-subset(co,specific_type=="Central co")

#split by year
byyear<-split(group_gen,group_gen$year)
#first year
first<-as.data.frame(byyear[1])
first_n<-nrow(first)

count_combn<-list()
count_list<-list(1625)
year_list<-list(1985)
store<-data.frame()
film_k<-data.frame()
for (year in 1:33){
  d<-as.data.frame(byyear[year])
  
  colnames(d)<-c("pindex","keyword","keyword_index","year","specific_type")
  #this year
  this_y<-d$year
  #save this year's data into a comprehensive dataframe
  store<-rbind(store,d)
  if (year>3){
    #previous year
    pre_y<-(d$year-3)[1]
    only3<-subset(store,year>pre_y & year<this_y)
    #old keywords
    old<-na.omit(d[(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ="pindex"]
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), by=list(`pindex`,`year`)]
      store_combn<-c(store_combn,combn)
      only3_combn<-subset(store_combn,year>pre_y & year<this_y)
    }
    #new combinations
    new_combn<-na.omit(d[!(unique(na.omit(combn))) %in% (unique(na.omit(only3_combn))),])
    #new keywords
    new<-nrow(d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    film<-d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),]
    count_film<-as.data.frame((count(film$pindex)))
    film_k<-rbind(film_k,count_film)
    
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
  else{
    #new keywords
    new<-length(unique(na.omit(d$keyword_index)))
    count_film<-as.data.frame((count(d$pindex)))
    film_k<-rbind(film_k,count_film)
    #new combinations
    old<-na.omit(d[unique(d$keyword_index),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ='pindex']
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), .(`pindex`)]
    }
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
}
count_d<-data.frame(unlist(count_list))
year_d<-data.frame(unlist(year_list))
combn_d<-data.frame(unlist(count_combn))

#aggregate keywords by film
colnames(film_k)<-c("pindex","freq")
film_k<-as.data.table(film_k)
#save to the comprehensive new keywords by film table
all_film<-rbind(all_film,film_k)

bind<-data.frame(cbind(year_d,count_d,combn_d))
colnames(bind)<-c("year","number of new keywords","number of new combinations")

bind<-rbind(bind[1,],bind[3:34,])
g4<-ggplot(data=bind,aes(x=year,y=`number of new keywords`))+geom_line()+ggtitle("Group Generalist")
g4

# hybrid ------------------------------------------------------------------

#merge hybrid
hybrid<-subset(producers_us,specific_type=="Hybrid co")
#split by year
byyear<-split(hybrid,hybrid$year)
#first year
first<-as.data.frame(byyear[1])
first_n<-nrow(first)

count_combn<-list()
count_list<-list()
year_list<-list()
store<-data.frame()
film_k<-data.frame()
colnames(first)
for (year in 1:33){
  d<-as.data.frame(byyear[year])
  
  colnames(d)<-c("pindex","keyword","keyword_index","year","specific_type")
  #this year
  this_y<-d$year
  #save this year's data into a comprehensive dataframe
  store<-rbind(store,d)
  if (year>3){
    #previous year
    pre_y<-(d$year-3)[1]
    only3<-subset(store,year>pre_y & year<this_y)
    #old keywords
    old<-na.omit(d[(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ="pindex"]
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), by=list(`pindex`,`year`)]
      store_combn<-c(store_combn,combn)
      only3_combn<-subset(store_combn,year>pre_y & year<this_y)
    }
    #new combinations
    new_combn<-na.omit(d[!(unique(na.omit(combn))) %in% (unique(na.omit(only3_combn))),])
    #new keywords
    new<-nrow(d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),])
    film<-d[!(unique(na.omit(d$keyword_index))) %in% (unique(na.omit(only3$keyword_index))),]
    count_film<-as.data.frame((count(film$pindex)))
    film_k<-rbind(film_k,count_film)
    
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    year_list<-c(year_list,thatyear)
  }
  else{
    #new keywords
    new<-length(unique(na.omit(d$keyword_index)))
    count_film<-as.data.frame((count(d$pindex)))
    film_k<-rbind(film_k,count_film)
    #new combinations
    old<-na.omit(d[unique(d$keyword_index),])
    old<-as.data.table(old)
    old[, min_two := .N > 1, by ='pindex']
    old = old[min_two==TRUE]
    if (nrow(old)>1){
      combn<-old[,as.data.table(t(combn(keyword_index,2))), .(`pindex`)]
    }
    #get the year
    thatyear<-d[year,4]
    
    count_combn<-c(count_combn,nrow(new_combn))
    count_list<-c(count_list,new)
    
    year_list<-c(year_list,thatyear)
  }
}
count_d<-data.frame(unlist(count_list))
year_d<-data.frame(unlist(year_list))
combn_d<-data.frame(unlist(count_combn))

#aggregate keywords by film
colnames(film_k)<-c("pindex","freq")
film_k<-as.data.table(film_k)
#save to the comprehensive new keywords by film table
all_film<-rbind(all_film,film_k)

bind<-data.frame(cbind(year_d,count_d,combn_d))
colnames(bind)<-c("year","number of new keywords","number of new combinations")

producers_us<-left_join(producers_us,all_film,by="pindex")
producers_us<-as.data.table(producers_us)

bind<-rbind(bind[1,],bind[3:34,])
g5<-ggplot(data=bind,aes(x=year,y=`number of new keywords`))+geom_line()+ggtitle("Group Generalist and Specialist")
g5
g6<-ggplot(data=bind,aes(x=year,y=`number of new combinations`))+geom_line()+ggtitle("Group Generalist and Specialist")
g6



# question 1b -------------------------------------------------------------


glm1<-glm.nb(new_key~count_central_co+count_peripheral_co+count_hybrid+total_box+op_y+subsidiary+factor(year),data=co, offset(total_film))
summary(glm1)

glm.nb(new_combn~count_central_co+count_peripheral_co+count_hybrid+total_box+op_y+subsidiary+year,data=co, offset(total_film))

# question 3 --------------------------------------------------------------
#The results sugguest that the longer the producer has been in operation, the higher probability that it will produce higher return
#get rid of the NANs in y
co$return[which(!is.finite(co$return))] = NA
lm1<-lm(return~count_central_co+count_peripheral_co+count_hybrid+total_box+op_y+subsidiary+factor(year),data=co)
summary(lm1)

# question 4a --------------------------------------------------------------
glm2<-glm.nb(new_key~count_central_solo+count_peripheral_solo+total_box+op_y+subsidiary+factor(year),data=co, offset(total_film))
summary(glm2)


# question 4b -------------------------------------------------------------
#Engage in collaborations can give producers more information to ensure the success of the production of the film,even if they can be financially risky.
#add the number of new keywords
glm3<-glm.nb(new_key~count_central_co+count_peripheral_co+count_hybrid+total_box+op_y+subsidiary+factor(year)+new_key,data=co, offset(total_film))
summary(glm3)


























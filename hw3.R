setwd("C:/Users/Jacky Yang/Desktop/Social Network/hw3")

# install.packages("plm")
# install.packages("pglm")
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
library(plm)
library(pglm)
warning=FALSE
options(warn = -1)
# question 1a -------------------------------------------------------------
#delete NAs
rain_nona<-rain[!(is.na(rain$spi) | rain$spi==""), ]

unique_district<-unique(district$district)
#get the unique national election years
unique<-sort(unique(district$year))
party_yearlist<-list()
district_list<-list()
avg_rainlist<-list()
sum_rainfall_list<-list()
interval_list<-list()
newparty_list<-list()
state_list<-list()
flood_list<-list()
drought_list<-list()
sum_fd_list<-list()
newcaste_list<-list()
newsocialist_list<-list()
newcommunist_list<-list()
newsecular_list<-list()
newnationalist_list<-list()
neweconomic_list<-list()
newliberal_list<-list()
newreligious_list<-list()
newethnic_list<-list()
newfarleft_list<-list()
newnationalscope_list<-list()
newstatescope_list<-list()
newregionalscope_list<-list()
newsubnational_list<-list()
newfarright_list<-list()
newfarming_list<-list()
newvoting_list<-list()
newtotal_list<-list()
newconcentration_list<-list()
#split the dataframe into lists of district
bydistrict<-split(district,district$district)
#loop over each district list
for (disno in 1:length(bydistrict)){
  dis<-names(bydistrict[disno])
  district_d<-as.data.frame(bydistrict[disno])
  colnames(district_d)<-c("state","district","year","new_parties","new_parties_caste","new_parties_socialist",
                          "new_parties_communist","new_parties_secular","new_parties_nationalist","new_parties_economic",
                          "new_parties_liberal","new_parties_religious","new_parties_ethnic","new_parties_farleft",
                          "new_parties_national_scope","new_parties_state_scope","new_parties_regional_scope",
                          "new_parties_subnational_scope","new_parties_farright","new_parties_farming","voting_participation",
                          "total_parties","political_concentration")
  #loop over each unique national election year
  for (uni in 1:length(unique)){
    #get the current year in the loop
    disuni_year<-unique[uni]
    #overlap situation
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
        #calculate if that year is flood or drought
        flood<-0
        drought<-0
        for (i in 1:nrow(nooverlap)){
          if (nooverlap[i,4]>1){
            flood<-flood+1
          }
          else if (nooverlap[i,4]<(-1)){
            drought<-drought+1
          }
        }
        #sum the years of flood or drought
        sum_fd<-sum(flood,drought)
        #calculate avg spi
        avg_rain<-sum(nooverlap$spi)/nrow(nooverlap)
        #calculate sum of raw rainfall
        sum_rainfall<-sum(nooverlap$rain)
        #calculate interval years
        interval<-disuni_year-pre_year
        if (!is.na(avg_rain)){
          #append the results to the list outside of the loop
          party_yearlist<-c(party_yearlist,disuni_year)
          avg_rainlist<-c(avg_rainlist,avg_rain)
          sum_rainfall_list<-c(sum_rainfall_list,sum_rainfall)
          interval_list<-c(interval_list,interval)
          district_list<-c(district_list,as.character(dis))
          newparty<-subset(district_d,year==disuni_year)
          #keep all the columns from the original district dataset
          newparty_list<-c(newparty_list,newparty[,4])
          newcaste_list<-c(newcaste_list,newparty[,5])
          newsocialist_list<-c(newsocialist_list,newparty[,6])
          newcommunist_list<-c(newcommunist_list,newparty[,7])
          newsecular_list<-c(newsecular_list,newparty[,8])
          newnationalist_list<-c(newnationalist_list,newparty[,9])
          neweconomic_list<-c(neweconomic_list,newparty[,10])
          newliberal_list<-c(newliberal_list,newparty[,11])
          newreligious_list<-c(newreligious_list,newparty[,12])
          newethnic_list<-c(newethnic_list,newparty[,13])
          newfarleft_list<-c(newfarleft_list,newparty[,14])
          newnationalscope_list<-c(newnationalscope_list,newparty[,15])
          newstatescope_list<-c(newstatescope_list,newparty[,16])
          newregionalscope_list<-c(newregionalscope_list,newparty[,17])
          newsubnational_list<-c(newsubnational_list,newparty[,18])
          newfarright_list<-c(newfarright_list,newparty[,19])
          newfarming_list<-c(newfarming_list,newparty[,20])
          newvoting_list<-c(newvoting_list,newparty[,21])
          newtotal_list<-c(newtotal_list,newparty[,22])
          newconcentration_list<-c(newconcentration_list,newparty[,23])
          #append the results to the list outside of the loop
          state_list<-c(state_list,as.character(newparty[,1]))
          flood_list<-c(flood_list,flood)
          drought_list<-c(drought_list,drought)
          sum_fd_list<-c(sum_fd_list,sum_fd)
        }
      }
    }
      #no overlap situation
      else{
        if ((dis %in% rain_nona$district) & (disuni_year %in% district_d$year)){
          #rain data with cutoff years and all district
          just_year<-subset(rain_nona,year<=disuni_year)
          #rain data with cutoff years and just one district
          dis_year<-subset(just_year,district==dis)
          #calculate avg spi
          avg_rain<-sum(dis_year$spi)/nrow(dis_year)
          #calculate sum of raw rainfall
          sum_rainfall<-sum(dis_year$rain)
          #calculate interval years
          interval<-6
          flood<-0
          drought<-0
          for (i in 1:nrow(dis_year)){
            if (dis_year[i,4]>1){
              flood<-flood+1
            }
            else if (dis_year[i,4]<(-1)){
              drought<-drought+1
            }
          }
          sum_fd<-sum(flood,drought)
          if (!is.na(avg_rain)) {
            party_yearlist<-c(party_yearlist,disuni_year)
            avg_rainlist<-c(avg_rainlist,avg_rain)
            sum_rainfall_list<-c(sum_rainfall_list,sum_rainfall)
            interval_list<-c(interval_list,interval)
            district_list<-c(district_list,as.character(dis))
            newparty<-subset(district_d,year==disuni_year)
            #keep all the columns from the original district dataset
            newparty_list<-c(newparty_list,newparty[,4])
            newcaste_list<-c(newcaste_list,newparty[,5])
            newsocialist_list<-c(newsocialist_list,newparty[,6])
            newcommunist_list<-c(newcommunist_list,newparty[,7])
            newsecular_list<-c(newsecular_list,newparty[,8])
            newnationalist_list<-c(newnationalist_list,newparty[,9])
            neweconomic_list<-c(neweconomic_list,newparty[,10])
            newliberal_list<-c(newliberal_list,newparty[,11])
            newreligious_list<-c(newreligious_list,newparty[,12])
            newethnic_list<-c(newethnic_list,newparty[,13])
            newfarleft_list<-c(newfarleft_list,newparty[,14])
            newnationalscope_list<-c(newnationalscope_list,newparty[,15])
            newstatescope_list<-c(newstatescope_list,newparty[,16])
            newregionalscope_list<-c(newregionalscope_list,newparty[,17])
            newsubnational_list<-c(newsubnational_list,newparty[,18])
            newfarright_list<-c(newfarright_list,newparty[,19])
            newfarming_list<-c(newfarming_list,newparty[,20])
            newvoting_list<-c(newvoting_list,newparty[,21])
            newtotal_list<-c(newtotal_list,newparty[,22])
            newconcentration_list<-c(newconcentration_list,newparty[,23])
            #append the results to the list outside of the loop
            state_list<-c(state_list,as.character(newparty[,1]))
            flood_list<-c(flood_list,flood)
            drought_list<-c(drought_list,drought)
            sum_fd_list<-c(sum_fd_list,sum_fd)
          }
        }
      }
  }
}
#change the lists into dataframe
avg_d<-data.frame(unlist(avg_rainlist))
no_naavg<-data.frame(avg_d[complete.cases(avg_d), ])

sum_d<-data.frame(unlist(sum_rainfall_list))
no_nasum<-data.frame(sum_d[complete.cases(sum_d), ])

sum_fd_d<-data.frame(unlist(sum_fd_list))
no_nafd<-data.frame(sum_fd_d[complete.cases(sum_fd_d), ])

cas_d<-data.frame(unlist(newcaste_list))
con_d<-data.frame(unlist(newconcentration_list))
total_d<-data.frame(unlist(newtotal_list))
voting_d<-data.frame(unlist(newvoting_list))
farming_d<-data.frame(unlist(newfarming_list))
farright_d<-data.frame(unlist(newfarright_list))
subnational_d<-data.frame(unlist(newsubnational_list))
regional_d<-data.frame(unlist(newregionalscope_list))
statescope_d<-data.frame(unlist(newstatescope_list))
natoinal_d<-data.frame(unlist(newnationalscope_list))
farleft_d<-data.frame(unlist(newfarleft_list))
ethnic_d<-data.frame(unlist(newethnic_list))
religious_d<-data.frame(unlist(newreligious_list))
liberal_d<-data.frame(unlist(newliberal_list))
economic_d<-data.frame(unlist(neweconomic_list))
nationalist_d<-data.frame(unlist(newnationalist_list))
secular_d<-data.frame(unlist(newsecular_list))
communist_d<-data.frame(unlist(newcommunist_list))
socialist_d<-data.frame(unlist(newsocialist_list))

interval_d<-data.frame(unlist(interval_list))
no_nainterval<-data.frame(interval_d[complete.cases(interval_d), ])

party_d<-data.frame(unlist(party_yearlist))
no_naparty<-data.frame(party_d[complete.cases(party_d), ])

district_d<-data.frame(unlist(district_list))
no_nadistrict<-data.frame(district_d[complete.cases(district_d), ])

newparty_d<-data.frame(unlist(newparty_list))
no_nanewparty<-data.frame(newparty_d[complete.cases(newparty_d), ])

state_d<-data.frame(unlist(state_list))
no_nastate<-data.frame(state_d[complete.cases(state_d), ])
#bind all the data
bind<-data.frame(cbind(no_naavg,no_nasum,no_naparty,no_nadistrict,no_nanewparty,no_nastate,no_nainterval,no_nafd,cas_d,
                       socialist_d,communist_d,secular_d,nationalist_d,economic_d,liberal_d,religious_d,ethnic_d,farleft_d,
                       natoinal_d,statescope_d,regional_d,subnational_d,
                       farright_d,farming_d,voting_d,total_d,con_d))
graph_d <- as.data.frame(lapply(bind, unlist))
graph_d<-graph_d[order(graph_d$party_d.complete.cases.party_d....),]
colnames(graph_d)<-c("Avg_SPI","Sum_Raw_Rainfall","Year","District","New_Founded_Party","State","Interval",
                     "Years of drought/flood","new_parties_caste","new_parties_socialist",
                     "new_parties_communist","new_parties_secular","new_parties_nationalist","new_parties_economic",
                     "new_parties_liberal","new_parties_religious","new_parties_ethnic","new_parties_farleft",
                     "new_parties_national_scope","new_parties_state_scope","new_parties_regional_scope",
                     "new_parties_subnational_scope","new_parties_farright","new_parties_farming","voting_participation",
                     "total_parties","political_concentration")
#plot the graph
ggplot(data=graph_d,aes(x=New_Founded_Party,y=Avg_SPI))+geom_point()+ylab("Avg SPI")+xlab("New Founded Party")      
  
# question 1b -------------------------------------------------------------
border<-read.csv("border_information.csv")
#cleaned the data to make sure "district" appear in "focal_district" as well
border2<-select(border,district,focal_district)
colnames(border2)<-c("focal_district","district")
combined<-unique(rbind(border,border2))

neighbor_list<-list()
#split the dataframe into lists of focal_district
neighbor<-split(combined,combined$focal_district)

for (row in 1:nrow(graph_d)){
  dis<-graph_d[row,4]
  spe_nei<-neighbor[as.character(dis)]
  spe_nei_d<-as.data.frame(spe_nei)
  inside_avg_sum<-c()
  inside_sum_sum<-c()
  inside_avg_df<-c()
  for (inside in 1:nrow(spe_nei_d)){
    #select one of the neighbors
    inside_nei<-spe_nei_d[inside,2]
    #select one of the neighbors' dataset from graph_d
    nei_avg_d<-subset(graph_d,District==as.character(inside_nei))
    #use only one relevant year
    right_year<-subset(nei_avg_d,Year==graph_d[row,3])
    #get the data fileds that I want to calculate
    nei_avg<-right_year[,1]
    nei_sum_avg<-right_year[,2]
    nei_avg_df<-right_year[,8]
    #avg of spi
    inside_avg_sum<-c(inside_avg_sum,nei_avg)
    #sum of rainfall
    inside_sum_sum<-c(inside_sum_sum,nei_sum_avg)
    #sum of df
    inside_avg_df<-c(inside_avg_df,nei_avg_df)
  }
  #sum of all the neighbor
  inside_avg_sum<-sum(inside_avg_sum)/length(inside_avg_sum)
  #rainfall
  inside_sum_sum<-sum(inside_sum_sum)/length(inside_sum_sum)
  #df
  inside_avg_df<-sum(inside_avg_df)/length(inside_avg_df)
  #add spi to graph_d
  graph_d[row,28]<-inside_avg_sum
  #add rainfall to graph_d
  graph_d[row,29]<-inside_sum_sum
  #add neighbor_df to graph_d
  graph_d[row,30]<-inside_avg_df
}

colnames(graph_d)<-c("Avg_SPI","Avg_Raw_Rainfall","Year","District","New_Founded_Party","State","Interval","Years_of_drought_flood",
                     "new_parties_caste","new_parties_socialist",
                     "new_parties_communist","new_parties_secular","new_parties_nationalist","new_parties_economic",
                     "new_parties_liberal","new_parties_religious","new_parties_ethnic","new_parties_farleft",
                     "new_parties_national_scope","new_parties_state_scope","new_parties_regional_scope",
                     "new_parties_subnational_scope","new_parties_farright","new_parties_farming","voting_participation",
                     "total_parties","political_concentration","Neighbor's Avg SPI","Neighbor's Avg Raw Rainfall","Neighbor_df")

graph_datatable<-as.data.table(graph_d)
#create lagged values
graph_datatable<-na.omit(setDT(graph_datatable)[, lag_spi:=c(0, Avg_SPI[-.N]), by="District"][, lag_rain:=c(0, `Avg_Raw_Rainfall`[-.N]), by="District"])
graph_datatable<-na.omit(setDT(graph_datatable)[, neighbor_lag_spi:=c(0, `Neighbor's Avg SPI`[-.N]), by="District"][, neighbor_lag_rain:=c(0, `Neighbor's Avg Raw Rainfall`[-.N]), by="District"])
#regression for Avg. SPI
plm1<-plm(Avg_SPI~lag_spi+neighbor_lag_spi+Interval,data=graph_datatable,effect="twoways",model="within",index="District")
summary(plm1)
#regression for Avg. Raw Rainfall
plm2<-plm(Avg_Raw_Rainfall~lag_rain+neighbor_lag_rain+Interval,data=graph_datatable,effect="twoways",model="within",index="District")
summary(plm2)

# question 1c -------------------------------------------------------------
##all the coefficients are insignificant now while the SPI and Raw Rainfall are significant coefficients previously.

#create lagged values
graph_datatable<-na.omit(setDT(graph_datatable)[, lag_df:=c(0, Years_of_drought_flood[-.N]), by="District"])
graph_datatable<-na.omit(setDT(graph_datatable)[, neighbor_lag_df:=c(0, Neighbor_df[-.N]), by="District"])

pglm1<-pglm(Years_of_drought_flood~lag_df+neighbor_lag_df+Interval, data=graph_datatable, 
            effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm1)
# question 2 -------------------------------------------------------------
##new_parties_communist and new_parties_socialist are the two types of parties that are more likely to be formed when a district
##experiences extreme weather as they have significant coefficients of number of years of drought/flood.
pglm2<-pglm(New_Founded_Party~Years_of_drought_flood+Interval, data=graph_datatable, 
            effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2)

#run regression on all parties to see if certain parties are more correlated with the predicator variables
pglm2_1<-pglm(new_parties_caste~Years_of_drought_flood+Interval, data=graph_datatable, 
            effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_1)
#run regression on all types of parties
pglm2_2<-pglm(new_parties_socialist~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_2)

pglm2_3<-pglm(new_parties_communist~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_3)

pglm2_4<-pglm(new_parties_secular~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_4)

pglm2_5<-pglm(new_parties_nationalist~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_5)

pglm2_7<-pglm(new_parties_liberal~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_7)

pglm2_8<-pglm(new_parties_religious~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_8)

pglm2_9<-pglm(new_parties_ethnic~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_9)

pglm2_10<-pglm(new_parties_farleft~Years_of_drought_flood+Interval, data=graph_datatable, 
              effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_10)

pglm2_11<-pglm(new_parties_national_scope~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_11)

pglm2_12<-pglm(new_parties_state_scope~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_12)

pglm2_13<-pglm(new_parties_regional_scope~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_13)

pglm2_14<-pglm(new_parties_subnational_scope~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_14)

pglm2_15<-pglm(new_parties_farright~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_15)

pglm2_16<-pglm(new_parties_farming~Years_of_drought_flood+Interval, data=graph_datatable, 
               effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm2_16)


# question 3 --------------------------------------------------------------
##lagged values of number of years of drought/flood in neighbouring districts affect the number of new parties being founded
##as it is a significant coefficient
pglm3<-pglm(New_Founded_Party~Years_of_drought_flood+neighbor_lag_df+Interval, data=graph_datatable, 
            effect = "twoways",model = "within", index = "District", family = "poisson")
summary(pglm3)


# question 4 --------------------------------------------------------------
##political concentration is not affected by extreme weather as regression deos not have siginificant coefficients
pglm4<-plm(political_concentration~Years_of_drought_flood+neighbor_lag_df+Interval,data=graph_datatable, 
            effect = "twoways",model = "within", index = "District")
summary(pglm4)

# question 5a  -------------------------------------------------------------
##Diffusion of political organizing has no relationship with the fact that the new parties have contested in their neighboring
##districts as no coefficients are significant.
party<-read.csv("new_parties_in_each_district_by_candidate.csv")
party_nona<-party[!(is.na(party$party_name) | party$party_name==""), ]
#split by district
bydistrict<-split(party_nona,party_nona$district)
#create lists to collect data output from the for loop
likelihood_list<-list()
year_list<-list()
dis_list<-list()
for (row in 1:length(bydistrict)){
  single_dis<-as.data.frame(bydistrict[row])
  colnames(single_dis)<-c("district","state","year","name","party_name","incumbent","win")
  single_dis<-single_dis[order(single_dis$year),]
  #loop over each district in district dataset
  for (dis_row in 1:nrow(single_dis)){
    thispar<-subset(party_nona,party_name==as.character(single_dis[dis_row,5]))
    if (single_dis[dis_row,3]==min(thispar$year)){
      n<-0
      likelihood_list<-c(likelihood_list,n)
      #append year
      year_list<-c(year_list,single_dis[dis_row,3])
      #append district
      dis_list<-c(dis_list,as.character(single_dis[dis_row,1]))
      next
    }
    else{
      #get this district's neighbors
      neighbor<-subset(combined,focal_district==as.character(single_dis[dis_row,1]))
      #get party name
      par<-as.character(single_dis[dis_row,5])
      #get this party's election history on other districts
      par_his<-subset(party_nona,party_name==as.character(par))
      ##select only relevant previous years
      #current year
      current<-single_dis[dis_row,3]
      par_his<-subset(par_his,year<current)
      #count how many time it appears in a neighbor district
      n<-length(which(par_his$district==as.character(neighbor$district)))
      #get the likelihood from total number election it has had
      likelihood<-n/nrow(par_his)
      likelihood_list<-c(likelihood_list,likelihood)
      #append year
      year_list<-c(year_list,single_dis[dis_row,3])
      #append district
      dis_list<-c(dis_list,as.character(single_dis[dis_row,1]))
      
    }
  }
}
#change the lists into dataframe
likelihood_d<-data.frame(unlist(likelihood_list))
no_nalikelihood<-data.frame(likelihood_d[complete.cases(likelihood_d), ])

year_d<-data.frame(unlist(year_list))
no_nayear<-data.frame(year_d[complete.cases(year_d), ])

dis_d<-data.frame(unlist(dis_list))
no_nadis<-data.frame(dis_d[complete.cases(dis_d), ])

bind<-data.frame(cbind(likelihood_d,year_d,dis_d))

likelihood_d <- as.data.frame(lapply(bind, unlist))

colnames(likelihood_d)<-c("likelihood","year","district")

all_d<-data.frame(graph_datatable)
#join likelihood table with the comprehensive table
joined<-left_join(all_d,likelihood_d,by = c("District"="district","Year"="year"))


#insignificant using the coefficient of lagged values of number of years of drought/flood in neighbors 
pglm5<-plm(likelihood~Years_of_drought_flood+neighbor_lag_df+Interval,data=joined, 
            effect = "twoways",model = "within", index = "District")
summary(pglm5)
#insignificant using the coefficient of number of years of drought/flood in neighbors 
pglm5_1<-plm(likelihood~Years_of_drought_flood+Neighbor_df+Interval,data=joined, 
            effect = "twoways",model = "within", index = "District")
summary(pglm5_1)
# question 5b -------------------------------------------------------------
##Diffusion of political organizing is not related with the fact that the new parties have not contested in their neighboring
##districts as no coefficients are significant.
bydistrict<-split(party_nona,party_nona$district)

likelihood_list<-list()
year_list<-list()
dis_list<-list()
for (row in 1:length(bydistrict)){
  single_dis<-as.data.frame(bydistrict[row])
  colnames(single_dis)<-c("district","state","year","name","party_name","incumbent","win")
  single_dis<-single_dis[order(single_dis$year),]
  #loop over each district in district dataset
  for (dis_row in 1:nrow(single_dis)){
    #select only this party's election history
    thispar<-subset(party_nona,party_name==as.character(single_dis[dis_row,5]))
    #if the party doesn't have any previous election history, consider the likelihood of having not contesting in its neighboring
    #district in previous elections as 1
    if (single_dis[dis_row,3]==min(thispar$year)){
      n<-1
      likelihood_list<-c(likelihood_list,n)
      #append year
      year_list<-c(year_list,single_dis[dis_row,3])
      #append district
      dis_list<-c(dis_list,as.character(single_dis[dis_row,1]))
      next
    }
    else{
      #calculate likelihood only the district we are targetting appear in the "border_information" file
      if (single_dis[dis_row,1] %in% combined$focal_district){
        #get this district's neighbors that this party has contested in
        neighbor<-subset(combined,focal_district==as.character(single_dis[dis_row,1]))
        #get party name
        par<-as.character(single_dis[dis_row,5])
        #get this party's election history on other districts
        par_his<-subset(party_nona,party_name==as.character(par))
        ##select only relevant previous years
        #current year
        current<-single_dis[dis_row,3]
        par_his<-subset(par_his,year<current)
        #count how many time it doesn't contest in a neighbor district
        n<-0
        for (i in 1:nrow(par_his)){
          if (!par_his[i,1]==as.character(neighbor$district)){
            n<-n+1
          }
        }
        #get the likelihood from total number election it has had
        likelihood<-n/nrow(par_his)
        likelihood_list<-c(likelihood_list,likelihood)
        #append year
        year_list<-c(year_list,single_dis[dis_row,3])
        #append district
        dis_list<-c(dis_list,as.character(single_dis[dis_row,1]))
      }
    }
  }
}
#change the lists into dataframe
likelihood_d<-data.frame(unlist(likelihood_list))
no_nalikelihood<-data.frame(likelihood_d[complete.cases(likelihood_d), ])

year_d<-data.frame(unlist(year_list))
no_nayear<-data.frame(year_d[complete.cases(year_d), ])

dis_d<-data.frame(unlist(dis_list))
no_nadis<-data.frame(dis_d[complete.cases(dis_d), ])

bind<-data.frame(cbind(likelihood_d,year_d,dis_d))

likelihood_d <- as.data.frame(lapply(bind, unlist))
colnames(likelihood_d)<-c("likelihood","year","district")


all_d<-data.frame(graph_datatable)
#join likelihood datatable with previous comprehensive datatable
joined<-left_join(all_d,likelihood_d,by = c("District"="district","Year"="year"))

#significant using the coefficient of the lagged values of number of years of drought/flood in neighbors 
pglm6<-plm(likelihood~Years_of_drought_flood+neighbor_lag_df+Interval,data=joined, 
            effect = "twoways",model = "within", index = "District")
summary(pglm6)
#insignificant using the coefficient of number of years of drought/flood in neighbors 
pglm6_1<-plm(likelihood~Years_of_drought_flood+Neighbor_df+Interval,data=joined, 
            effect = "twoways",model = "within", index = "District")
summary(pglm6_1)












  
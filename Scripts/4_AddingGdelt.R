#Combine with the other data and add variables that I will use for figures and analysis. 
library(tidyverse)
library(stringi)


######Joining GDELT DATA with VADER results######

country<-c("Mexico", "US")

gdeltDATA <- read_csv("~/gdeltDATA.csv")
name<-c("MX", "US")
#Select variables from tweets data
#MEXICO
for (i in c(1,2)){
  if (i==1){
    tweets<-tweets_MX
  } else {
    tweets<-tweets_US
  }
  #rename vader
  names(tweets)[16]<-"vader_compound"
  names(tweets)[17]<-"vader_neu"
  names(tweets)[18]<-"vader_neg"
  names(tweets)[19]<-"vader_pos"
  
  tweets<-tweets %>% mutate(day_time = as.POSIXlt(tweet_date)) %>% 
  mutate(hour_tweet = hour(tweet_date)) %>%
    mutate(time_period = case_when(
      hour_tweet > 06 & hour_tweet < 12 ~ "Morning",
      TRUE ~ "Afternoon"))
      #hour_tweet >= 12 & hour_tweet < 17 ~ "Afternoon",
      #hour_tweet >= 17 & hour_tweet < 21 ~ "Evening",
      #hour_tweet >=21 | hour_tweet <= 6 ~ "Night"))

  
gdelt<- gdeltDATA %>% filter(country %in% country[i]) %>% 
  mutate(mig_vol=mig_news/tot_news,
         day = as.Date(day, format = "%Y-%m-%d")) %>% 
  select(day, ave_tone, mig_vol, mig_news)

#Year variable
data<-tweets %>%
  mutate(day = as.Date(day, format = "%Y-%m-%d"),
         state_name =stri_trans_general(state_name,"Latin-ASCII")) %>% 
  select(tweet_id, author_id, day, state_name, vader_compound, hour_tweet, tweet_date) %>% 
  left_join(gdelt, by="day") %>% 
  rename(vader=vader_compound) %>% 
  mutate(period =  case_when(day < ymd('2018-10-12') ~ 1,
                             day >= ymd('2018-10-12') & day <  ymd('2018-12-08') ~ 2,
                             day >= ymd('2018-12-08') & day <  ymd('2019-07-17') ~ 3,
                             day >= ymd('2018-07-17') & day <= ymd('2019-12-31') ~ 4,
                             day >= ymd('2020-01-01') & day <= ymd('2020-12-31') ~ 5,
                             day >= ymd('2021-01-01') & day <= ymd('2021-12-31') ~ 6),
         year = case_when(day >= ymd('2018-01-01') & day <=  ymd('2018-12-31') ~ "2018",
                          day >= ymd('2019-01-01') & day <=  ymd('2019-12-31') ~ "2019",
                          day >= ymd('2020-01-01') & day <=  ymd('2020-12-31') ~ "2020",
                          day >= ymd('2021-01-01') & day <=  ymd('2021-12-31') ~ "2021"),
         period2 =  case_when(day < ymd('2018-10-12') ~ "1",
                              day >= ymd('2018-10-12') & day <  ymd('2018-12-08') ~ "2",
                              day >= ymd('2018-12-08') & day <= ymd('2019-12-31') ~ "3", 
                              day >= ymd('2020-01-01') & day <= ymd('2020-12-31') ~ "4",
                              day >= ymd('2021-01-01') & day <= ymd('2021-12-31') ~ "5"),
         vader_d = case_when(vader <= -0.05 ~ "Negative",
                             vader > -0.05 & vader < 0.05 ~ "Neutral",
                             vader >= 0.05 ~ "Positive"),
         vader_d2 = case_when(vader <= -0.5  ~ "Strongly negative",
                              vader > -0.5 & vader <= -0.05 ~ "Moderately negative",
                              vader > -0.05 & vader < 0.05 ~ "Neutral", 
                              vader >= 0.05 & vader < 0.5 ~ "Moderately positive",
                              vader >= 0.5 ~ "Strongly positive"))

#Variable of region in border or not for Mexico and the US. 
  if (i == 1){
    
  data<- data %>% mutate(region_b = case_when(state_name %in% c("Baja California", "Sonora", 
                                             "Chihuahua", "Coahuila", "Nuevo León", 
                                             "Tamaulipas") ~ "North border",
                                state_name %in% c("Chiapas", "Tabasco", "Quintana Roo",
                                             "Campeche") ~ "South border",
                                TRUE ~ "Other"))
  name_file<-"data_MX"
  assign(name_file,data)
  
  } else if (i == 2) {
    data<- data %>% mutate(region_b = case_when (state_name %in% c("California", "Arizona", 
                                       "New Mexico", "Texas") ~ "Border",
                          TRUE ~ "Other"))
  }
  name_file<-"data_US"
  assign(name_file,data)
  
}

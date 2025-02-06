#Regressions
library(stargazer)
library(lattice)
library(lme4) 
library(lmerTest)


tweets_MX <- tweets_MX  %>% select(tweet_id, tweet_lang)
data_analysis_MX_2018<-data_MX_2018rates %>% full_join(tweets_MX) %>% distinct(tweet_id, .keep_all = TRUE)

tweets_US <- tweets_US  %>% select(tweet_id, tweet_lang)
data_analysis_US_2018<-data_US_2018rates %>% left_join(tweets_US) %>% distinct(tweet_id, .keep_all = TRUE)

#I added some laggeed variables, because at some point we thought about using them but at the end we did not. 
#However they ended up being in the sample 

#Create variable for lagged effect 
#effect of the dat before
#effect for moring and evening
gdeltDATA <- read_csv("gdeltDATA.csv")
###########MEXICO##########################################################
gdelt_day_lag<-gdeltDATA %>% filter(country== "Mexico") %>% 
  mutate(mig_vol=mig_news/tot_news) %>%
  select(day, ave_tone, mig_news, mig_vol) %>% 
  distinct(day, .keep_all = TRUE) %>% 
  rename(day_lag = day)

gdelt_day_time_lag<-gdelt_day_lag %>% rename(day_time_lag = day_lag)

data_analysis_MX_2018<- data_analysis_MX_2018 %>%
  mutate(time_period = case_when( hour_tweet > 06 & hour_tweet < 12 ~ "Morning",
                                  TRUE ~ "Afternoon")) %>% 
  mutate(day_time_lag = case_when(time_period=="Morning" ~ day - 1,
                                  time_period=="Afternoon" ~ day), 
         day_lag = day - 1) %>% 
  left_join(gdelt_day_time_lag, by = c("day_time_lag")) %>% 
  rename(ave_tone = ave_tone.x,
       mig_vol = mig_vol.x, 
       mig_news = mig_news.x,
       ave_tone_day_time_lag = ave_tone.y,
       mig_vol_day_time_lag = mig_vol.y ,
       mig_news_day_time_lag = mig_news.y) %>% 
  left_join(gdelt_day_lag, by = "day_lag") %>%
  rename(ave_tone = ave_tone.x,
         mig_vol = mig_vol.x, 
         mig_news = mig_news.x,
         ave_tone_day_lag = ave_tone.y,
         mig_vol_day_lag = mig_vol.y,
         mig_news_day_lag = mig_news.y) %>% 
  mutate(duplicate = duplicated(tweet_id), 
         covid = case_when(day > as.Date("2021-03-21", format = "%Y-%m-%d") ~ "covid",
                           TRUE ~ "before"), 
         usgov = case_when(day > as.Date("2021-01-20", format = "%Y-%m-%d") ~ "biden",
                           TRUE ~ "Trump")) %>% 
  mutate(month = format(as.Date(data_analysis_MX_2018$day), "%Y-%m"))

#21 marzo 2020  cierre de la frontera terreste de US

###################US######################
gdelt_day_lag<-gdeltDATA %>% filter(country== "US") %>% 
  mutate(mig_vol=mig_news/tot_news) %>%
  select(day, ave_tone, mig_news, mig_vol) %>% 
  distinct(day, .keep_all = TRUE) %>% 
  rename(day_lag = day)

gdelt_day_time_lag<-gdelt_day_lag %>% rename(day_time_lag = day_lag)

data_analysis_US_2018<- data_analysis_US_2018 %>%
  mutate(time_period = case_when( hour_tweet > 06 & hour_tweet < 12 ~ "Morning",
                                  TRUE ~ "Afternoon")) %>% 
  mutate(day_time_lag = case_when(time_period=="Morning" ~ day - 1,
                                  time_period=="Afternoon" ~ day), 
         day_lag = day - 1) %>% 
  left_join(gdelt_day_time_lag, by = c("day_time_lag")) %>% 
  rename(ave_tone = ave_tone.x,
         mig_vol = mig_vol.x, 
         mig_news = mig_news.x,
         ave_tone_day_time_lag = ave_tone.y,
         mig_vol_day_time_lag = mig_vol.y ,
         mig_news_day_time_lag = mig_news.y) %>% 
  left_join(gdelt_day_lag, by = "day_lag") %>%
  rename(ave_tone = ave_tone.x,
         mig_vol = mig_vol.x, 
         mig_news = mig_news.x,
         ave_tone_day_lag = ave_tone.y,
         mig_vol_day_lag = mig_vol.y,
         mig_news_day_lag = mig_news.y) %>% 
  mutate(duplicate = duplicated(tweet_id), 
         covid = case_when(day > as.Date("2021-03-21", format = "%Y-%m-%d") ~ "covid",
                           TRUE ~ "before"), 
         usgov = case_when(day > as.Date("2021-01-20", format = "%Y-%m-%d") ~ "biden",
                           TRUE ~ "Trump")) %>% 
  mutate(month = format(as.Date(data_analysis_US_2018$day), "%Y-%m"))

#I kept only final data for Mexico and the US
#21 marzo 2020  cierre de la frontera terreste de US
####When looking at the absolute value of vader





















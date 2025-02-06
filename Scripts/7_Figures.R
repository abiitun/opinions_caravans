#Figures using the final sample
load("~/EDSDthesis/analysis/CarMigTWEETS_data_analysisIMMIG_130423.RData")
load("~/EDSDthesis/analysis/CarMig_data_analysis_final_110523.RData")


library(ggplot2)
library(mxmaps)
library(usmap)
library(readxl)
library(tidyverse)

mx_states <- read_excel("~/states_namesIPUMS.xlsx", 
                        sheet = "Mx_STATES") %>% select(state_name, state)

#Average sentiment per day
#Put in the same 

#Average sentiment per tweet
daily_sent_MX<-data_analysis_MX_2018  %>% select(day, vader) %>% 
#daily_sent_MX<-data_MX_2018rates %>% select(day, vader) %>% 
  #mutate(day = as.Date(day, format= "%Y-%m-%d"),
  #       vader = `vader_MX$compound` ) %>% 
  group_by(day) %>% 
  summarise(Mexico = mean(vader)) %>% 
  ungroup()

daily_sent_US<-data_analysis_US_2018 %>% select(day, vader) %>% 
#daily_sent_US<-data_US_2018rates %>% select(day, vader) %>% 
  #mutate(day = as.Date(day, format= "%Y-%m-%d"),
  #       vader = `vader_MX$compound` ) %>% 
  group_by(day) %>% 
  summarise(US = mean(vader)) %>% 
  ungroup() 


daily_sent<-full_join(daily_sent_MX, daily_sent_US, by="day") 

ggplot(aes(x=day), data = daily_sent) +
  geom_point(aes(y=Mexico))

daily_sent_2<-daily_sent %>% pivot_longer(2:3,names_to = "country",values_to = "daily_ave_sent")

png("~/ave_sent_BOTH.png",width = 10, height = 7,units='in', res= 600)
ggplot(aes(x=day), data = daily_sent_2) +
# geom_point(aes(y=US))
  geom_point(aes(y = daily_ave_sent, colour = country, shape=country, size=country)) +
  #geom_point(aes(y = Mexico, colour = "Mexico", shape="Mexico"))+
  scale_colour_manual(values=c("Mexico" = "darkorange3", "US"= "darkblue"))+
  scale_size_manual(values=c(2,2))+
  scale_shape_manual(values=c(17,16))+
  ylab("Tweets' daily average sentiment score")+
  #theme_minimal()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title=element_blank(),
        legend.position = "bottom")
dev.off()
  





ggplot(aes(x=vader)) +
  geom_histogram(data=data_analysis_MX_2018, alpha=0.2, fill="darkorange3")+
  geom_histogram(data=data_analysis_US_2018, alpha=0.3, fill="darkblue")

d_mx<-data_analysis_MX_2018 %>% select(vader) 
d_us<-data_analysis_US_2018 %>% select(vader)
d_mx$country <- 'Mexico'
d_us$country <- 'US'
d<-rbind(d_mx, d_us)

tiff("~/hist_sent_both.tiff", units="cm", width=9, height=7, res=300, pointsize=6)
ggplot(d, aes(vader, fill=country)) + 
  #geom_histogram(alpha = 0.6, aes(y = ..count..),position = 'dodge')+
  geom_histogram(alpha = 0.6,position = 'dodge')+
  #scale_color_manual(values=c("Mexico"="darkorange", "US"="darkblue"))+
  scale_fill_manual(values=c("Mexico"="darkorange", "US"="darkblue"))+
  #scale_y_continuous(breaks=seq(0,5,by=2), trans="")+
  xlab("VADER sentiment")+
  ylab("Number of tweets")+
  theme_minimal()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        legend.title=element_blank(),
        legend.position = "bottom")
dev.off()



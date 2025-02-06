
library(haven)
library(readxl)

names<-c("MX", "US")

MX_states <- read_excel("states_namesIPUMS.xlsx", 
                             sheet = "Mx_STATES")
US_states <- read_excel("states_namesIPUMS.xlsx", 
                             sheet = "US_STATES")

#Data using EIC 2015 (data downloaded from INEGI)
IMMIG_RATES_eic2015<-read_dta("IMMIG_RATES_eic2015.dta")
#Data from mexican census 2020
IMMIG_RATES_censo2020 <- read_dta("IMMIG_RATES_censo2020.dta")

IMMIG_RATES_eic2015<-IMMIG_RATES_eic2015 %>% 
  mutate(state=as.integer(ent)) %>%
  left_join(MX_states, by = "state") %>% 
  select(-c(ent,code,state))

IMMIG_RATES_censo2020<-IMMIG_RATES_censo2020 %>% 
  mutate(state=as.integer(ent)) %>%
  left_join(MX_states, by = "state") %>% 
  select(-c(ent,code,state))  

data_MX_2018rates_1<- data_MX %>% 
  filter(year==2018 | year == 2019) %>% 
  left_join(IMMIG_RATES_eic2015, by="state_name")
data_MX_2018rates_2<- data_MX %>% 
    filter(year==2020 | year == 2021) %>% 
  left_join(IMMIG_RATES_censo2020, by="state_name")
  
data_MX_2018rates<-rbind(data_MX_2018rates_1,data_MX_2018rates_2)


#Data from ACS by years for US
library(haven)
IMMIG_RATES_2018 <- read_dta("~/IMMIG_RATES_2018.dta")
IMMIG_RATES_2019 <- read_dta("~/IMMIG_RATES_2019.dta")
IMMIG_RATES_2020 <- read_dta("~/IMMIG_RATES_2020.dta")
IMMIG_RATES_2021 <- read_dta("~/IMMIG_RATES_2021.dta")
library(labelled)

IMMIG_RATES_2018 <- IMMIG_RATES_2018 %>% mutate(year=2018) %>% 
  filter(statefip <= 56) %>% 
  mutate(state = remove_labels(statefip)) %>% 
  left_join(US_states, by="state") %>% 
  select(-c(statefip, state, code))

IMMIG_RATES_2019 <- IMMIG_RATES_2019 %>% mutate(year=2019) %>% 
  filter(statefip <= 56) %>% 
  mutate(state = remove_labels(statefip)) %>% 
  left_join(US_states, by="state") %>% 
  select(-c(statefip,state,code))

IMMIG_RATES_2020 <- IMMIG_RATES_2020 %>% mutate(year=2020) %>% 
  filter(statefip <= 56) %>% 
  mutate(state = remove_labels(statefip)) %>% 
  left_join(US_states, by="state") %>% 
  select(-c(statefip,state,code))

IMMIG_RATES_2021 <- IMMIG_RATES_2021 %>% mutate(year=2021) %>% 
  filter(statefip <= 56) %>% 
  mutate(state = remove_labels(statefip)) %>% 
  left_join(US_states, by="state") %>% 
  select(-c(statefip,state,code))

IMMIG_RATES_years<-bind_rows(IMMIG_RATES_2018,IMMIG_RATES_2019,IMMIG_RATES_2020,IMMIG_RATES_2021)
IMMIG_RATES_years<-IMMIG_RATES_years %>% mutate(year=as.character(year))

data_US_2018rates<-data_US %>% left_join(IMMIG_RATES_years, by = c("year", "state_name"))


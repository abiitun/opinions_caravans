
#Libraries
library(readr)
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(forcats)
library(tidyverse)
library(viridis)
library(lubridate)
library(stringr)
library(sf)
library(spData)
library(googleLanguageR)
library(vader)
library(sf)
library(spData)

tweets_total <- read.csv("TWEETS.txt", encoding="UTF-8", header=FALSE)
tweets<- tweets_total

####################
#Locations
#First we found the easy locations

length(unique(tweets$tweet_place))
#[1] 2573

length(unique(tweets$tweet_geo))
#[1] 2579

#Create a file with only the locations.
tweet_locations<-tweets %>% count(tweet_place)
tweet_locations_geo<-tweets %>% count(tweet_place, tweet_geo)
#List of states in Mexico and list of sections in mexico city so that we can match with the text
ListMX<- c( "Distrito Federal",    "Guanajuato",          "Chiapas",            
            "Guerrero",            "Puebla",              "Veracruz",                         
            "Coahuila" ,           "Sonora",              "Aguascalientes",      "Sinaloa",            
            "Chihuahua",          "Tamaulipas",          "Querétaro",           "Michoacán",          
            "Tlaxcala",            "Nuevo León",          "Jalisco",            "Hidalgo",            
            "Morelos",             "Nayarit",             "Baja California",     "Quintana Roo",       
            "Campeche",            "Tabasco",             "San Luis Potosí",     "Colima",             
            "Baja California Sur", "Oaxaca",              "Durango",             "Zacatecas",          
            "Yucatán")
List_cdmx<- c( "Álvaro Obregón", "Azcapotzalco", "Benito Juárez", "Coyoacán", "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero", 
               "Iztacalco", "Iztapalapa", "La Magdalena Contreras", "Magdalena Contreras", "Miguel Hidalgo", "Milpa Alta", "Tláhuac", "Tlalpan",
               "Venustiano Carranza",
               "Xochimilco")

#List of US states
US_statesList <- read_excel("US_statesList.xlsx")
US_statesList <-US_statesList %>% 
  select(-`Postal Abbreviation`) %>% 
  rename(last_query = `Postal Code` )
US_statesList <- US_statesList %>% rename(state_name = `State Name/District`)
names(US_statesList)[2]<-"state_query"

#List of US cities with states
uscities <- read.csv("~/analysis/uscities.csv")
uscities <- uscities %>%  select(city, state_id, state_name)

#Separate location name by the comma, because then I can keep the abreviation of the state
#Many names are like New York, NY
tweet_locations<-tweet_locations %>% 
  rowwise() %>% 
  mutate(last_query = toString(unlist(strsplit(tweet_place, ","))[2]),
         first_query =toString(unlist(strsplit(tweet_place, ","))[1])) %>% 
  ungroup()

#I have both cases, when there is an abbreviation and when there is not.
#When there is Abbreviation I will match with my lists of abbreviations of states.
#In case of locations in Mexico, there names don't have abbreviations, but rather long names
tweet_locations<-tweet_locations %>%
#tweet_locations_geo<-tweet_locations_geo %>%
  mutate( last_query = str_trim(last_query, "left")) %>% 
  rowwise() %>% 
  mutate(state_name = case_when(last_query %in% ListMX ~ last_query,
                                last_query %in% List_cdmx ~ "Distrito Federal",
                                last_query %in% c("México") & first_query %in% ListMX ~ first_query,
                                last_query %in% c("México") & !(first_query %in% ListMX) ~ "México",
                                str_detect(last_query, "Tijuana") ~ "Baja California",
                                str_detect(last_query, "Monterrey") ~ "Nuevo León",
                                str_detect(last_query, "Guadalajara") ~ "Jalisco",
                                str_detect(last_query, "Mérida") ~ "Yucatán",
                                str_detect(last_query,"Tlatelolco") ~ "Distrito Federal", 
                                str_detect(last_query, "León") ~ "Guanajuato",
                                str_detect(last_query, "Oaxaca") ~ "Oaxaca",
                                str_detect(last_query, "Hidalgo") ~ "Hidalgo",
                                str_detect(last_query, "Detroit") ~ "Michigan",
                                str_detect(last_query, "Hermosillo") ~ "Sonora",
                                str_detect(last_query, "Tmps") ~ "Tamaulipas",
                                str_detect(last_query, "Veracruz de Ignacio de la Llave") ~ "Veracruz",
                                str_detect(last_query, "Michoacán de Ocampo") ~ "Michoacán",
                                str_detect(last_query, "Coahuila de Zaragoza") ~ "Coahuila",
                                str_detect(last_query, "Querétaro Arteaga") ~ "Querétaro",
                                last_query == "USA" ~ toString(unlist(strsplit(tweet_place, ","))[1]),
                                last_query  %in% uscities$city ~ uscities$state_name[match(last_query, uscities$city)],
                                last_query %in% US_statesList$state_name ~ US_statesList$state_name[match(last_query, US_statesList$state_name)],
                                last_query %in% US_statesList$state_query ~ US_statesList$state_name[match(last_query, US_statesList$state_query)])) %>% 
  ungroup()
aggregate(n ~ !is.na(state_name), data=tweet_locations, sum)
#is.na(state_name)     n
#1              FALSE  1748
#2               TRUE 10762

#Remeber, I have only tweets in english and in spanish that come from non verified accounts
sum(is.na(tweet_locations$state_name))
#[1] 543


############Locations that were not easily encountered.###############
geo_id<-tweets %>% select(tweet_place, tweet_geo) %>% unique()
other_locations<-tweet_locations %>%  filter(is.na(state_name))
other_loc<- other_locations %>% select(tweet_place) %>% left_join(geo_id, by="tweet_place") %>% unique()
#other_loc %>% select(tweet_geo) %>% filter(!is.na(tweet_geo)) %>%  write_csv( file = "other_tweetGeo_MigCaravan.csv")

#Using the API i retrieved the lattitude and longitude of each geo_id for all of the tweets in my sample. Code in python

#Upload coords
#I will paste it with others 
#I have all locations (worldwide) I will match lon and lat with state and keep for Mexico and the US
coords_ALL_MigCar <- read.csv("coords_tweets.txt", header=FALSE)
names(coords_ALL_MigCar)[1]<-"tweet_geo"
tweet_locations_geo_coords<-tweet_locations_geo %>%
  filter(is.na(state_name)) %>% 
  left_join(coords_ALL_MigCar, by="tweet_geo" )

tweet_locations_geo_coords<- tweet_locations_geo_coords %>% left_join(dfr, by=c("V2","V3"))

#function to go from longitude and lattitude to State. Code take from:
#https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

dfr<-coords_ALL_MigCar %>% select(V2, V3)
USA_gadm <- st_read(dsn = "gadm36_USA.gpkg", layer = "gadm36_USA_1")
MEX_gadm <- st_read(dsn = "gadm36_MEX.gpkg", layer = "gadm36_MEX_1")

dfr$tweetplace_US<-lonlat_to_state(dfr, states = USA_gadm, name_col ="NAME_1")
dfr$tweetplace_MEX<-lonlat_to_state(dfr, states = MEX_gadm, name_col ="NAME_1")
dfr<- dfr %>% rename(lon= V2,
                     lat = V3)
coords_ALL_MigCar <- coords_ALL_MigCar %>% rename(lon=V2,
                                                  lat=V3)
coords_ALL_MigCar<-coords_ALL_MigCar %>% full_join(dfr, by = c("lon", "lat"))
#coords_ALL_MigCar<- coords_ALL_MigCar %>% rename(tweet_geo=V1)

coords_to_paste<- coords_ALL_MigCar %>%
  filter(!is.na(tweetplace_MEX) | !is.na(tweetplace_US)) %>% 
  mutate(state_name = case_when(!is.na(tweetplace_US) ~ tweetplace_US,
                                !is.na(tweetplace_MEX) ~ tweetplace_MEX)) %>% 
  select(tweet_geo,state_name)

#paste back to tweets
#Remove Observations that have no NA

#Two separate files because I already have two separate tweets data files for US and MX
coords_mx<- coords_ALL_MigCar %>% filter(!is.na(tweetplace_MEX)) %>% 
  select(tweet_geo,tweetplace_MEX) %>% rename(state_name = tweetplace_MEX)
  

coords_us<- coords_ALL_MigCar %>% filter(!is.na(tweetplace_US)) %>% 
  select(tweet_geo,tweetplace_US) %>% rename(state_name = tweetplace_US)

#I have to append it to the "easy found" locations.

found_tweet_loc<-tweet_locations %>% 
  select(tweet_place, state_name) %>% 
  filter(!is.na(state_name)) %>%
  left_join(geo_id, by = "tweet_place") %>% 
  select(tweet_geo, state_name) %>% 
  rbind(coords_to_paste) %>% 
  unique()
#2042Locations found
#tweet_locations was 2573

#Paste back to tweets 
tweets<- tweets %>%  left_join(found_tweet_loc, by = c( "tweet_geo"))


#Separate by country
tweets<- tweets %>% 
  mutate(country = case_when(state_name %in% ListMX ~ "Mexico",
                             state_name %in% US_statesList$state_name ~ "US",
                             TRUE ~ "Other"))
table(tweets$country)
#Mexico  Other     US 
#3972   1751   6787 
#Mexico  Other     US 
#3813   1866   6831 

#Remove other countries
tweets_withLoc<-tweets %>% filter(country %in% c("Mexico", "US"))


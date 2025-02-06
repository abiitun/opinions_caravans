#Code to clean text and calculate VADER score
#####################Clean text################################
#Function to clean text
clean_textF<- function(text){
  text %>% str_remove_all("\n") %>% 
    str_replace("RT @[a-z,A-Z]*: ","") %>% 
    str_replace_all("@[[:alnum:]_]{4,}", "@anonymous") %>% 
    str_replace_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " http://url_removed")
}

#Tweet NAs for state name; those tweets for which I was not able to identify the state
tweets_withLoc <- tweets_withLoc %>% 
  mutate(clean_text = clean_textF(tweet_text)) %>% 
  mutate(only_url = case_when(clean_text %in% " http://url_removed" ~ 1,
                              TRUE ~ 0))

table(tweets_withLoc$only_url)
#    0     1 
#10548    96  


#Remove those that have nothing to do with migrant caravan

#Clean text, so that it will not take into account the names of the users.
tweets_withLoc<-tweets_withLoc %>% mutate(remove = case_when(str_detect(tweet_text, "(?i)servicio de (?i)internet") |
                                                               str_detect(tweet_text, "(?i)mariposa") |
                                                               str_detect(tweet_text, "(?i)fibra (?i)optica") |
                                                               str_detect(tweet_text, "fibra óptica") |
                                                               str_detect(tweet_text, "Periplos") |
                                                               str_detect(tweet_text, "ballena") |
                                                               str_detect(tweet_text, "bird")  |
                                                               str_detect(clean_text, "ornithology") |
                                                               str_detect(clean_text, "#wildlife") |
                                                               str_detect(clean_text, "#deer") |
                                                               str_detect(clean_text, "#antlers") |
                                                               str_detect(clean_text, "#nature") |
                                                               str_detect(clean_text, "whale") |
                                                               str_detect(clean_text, "dolphin") |
                                                               str_detect(clean_text, "github") |
                                                               str_detect(clean_text, "Azure") |
                                                               str_detect(clean_text, "Microsoft") |
                                                               str_detect(clean_text, "Ubuntu") |
                                                               str_detect(clean_text, "Python") |
                                                               str_detect(clean_text, "SQL") |
                                                               str_detect(clean_text, "bigdata") |
                                                               str_detect(clean_text, "big data") |
                                                               str_detect(clean_text, "virtual machine") ~ 1,
                                                             TRUE ~ 0 ))

#Paste translated text and clean text and vader score. Translate and clean for those that are missing
table(tweets_withLoc$remove)
#0     1 
#10638     6

tweets_data<- tweets_withLoc %>% filter(only_url==0 & remove == 0) %>% 
  select(tweet_id, tweet_date, tweet_text, tweet_lang, tweet_place, tweet_geo, author_id, 
         user_loc, user_tweet_counts, day, hashtags, state_name, clean_text, country)


#########Translate tweets###############################################
#Tweets in Spanish were translated in English
library(googleLanguageR)
#library(googleAuthR);

#stablish the authentification
#gar_auth_configure(path="client.json")
gl_auth("authentification.json")

#Test of function
a<-gl_translate(MX_tweets$clean_text[1], target = "en")$translatedText

#Translate tweets in spanish
MX_tweets<-MX_tweets %>% mutate(clean_text_en = case_when(tweet_lang=="en" ~ clean_text,
                                                          TRUE~gl_translate(clean_text, target = "en")$translatedText))

MX_tweets$clean_text_en <- rep(0, length(MX_tweets$clean_text))
for (i in 1:length(MX_tweets$clean_text)){
  if (MX_tweets$tweet_lang[i] == "es") {
    MX_tweets$clean_text_en[i]<-gl_translate(MX_tweets$clean_text[i], target = "en")$translatedText 
  } else {
    MX_tweets$clean_text_en[i]<-MX_tweets$clean_text[i]
  }
}

US_tweets$clean_text_en <- rep(0, length(US_tweets$clean_text))
for (i in 1:length(US_tweets$clean_text)){
  if (US_tweets$tweet_lang[i] == "es") {
    US_tweets$clean_text_en[i]<-gl_translate(US_tweets$clean_text[i], target = "en")$translatedText 
  } else {
    US_tweets$clean_text_en[i]<-US_tweets$clean_text[i]
  }
}


###########Vader sentiment #####################
library(vader)

vader_US<- vader_df(US_tweets$clean_text_en)
vader_MX<- vader_df(MX_tweets$clean_text_en)

#Run vader with no neutral words
vader_US_noneu<- vader_df(US_tweets$clean_text_en, neu_set = F)
vader_MX_noneu<- vader_df(MX_tweets$clean_text_en, neu_set = F)

#Run vader sentiment analysis
US_tweets_sent <- cbind(US_tweets, 
                        vader_US$compound,
                        vader_US$neu, 
                        vader_US$neg, 
                        vader_US$pos)

MX_tweets_sent <- cbind(MX_tweets, 
                        vader_MX$compound,
                        vader_MX$neu, 
                        vader_MX$neg, 
                        vader_MX$pos)


#Keeping selected variables
MX_tweets_sent <-MX_tweets_sent %>% 
  select(tweet_id, author_id, clean_text_en, `vader_MX$compound`,`vader_MX$neu`,`vader_MX$neg`,`vader_MX$pos`)
US_tweets_sent <-US_tweets_sent %>% 
  select(tweet_id, author_id,clean_text_en, `vader_US$compound`,`vader_US$neu`,`vader_US$neg`,`vader_US$pos`)

#Match with tweet_id and see what matches and what does not.
tweets_MX<-tweets_data %>% 
  filter(country %in% "Mexico") %>% 
  left_join(MX_tweets_sent, by= c("tweet_id", "author_id")) %>% 
  unique()
tweets_US<- tweets_data %>% filter(country %in% "US") %>% 
  left_join(US_tweets_sent, by= c("tweet_id", "author_id")) %>% 
  unique()





#Topic modelling
library(tidyverse)
#Code from https://rpubs.com/arafath/twitter_analysis

#count(unique(tweets_MX$tweet_id))
#count(unique(tweets_US$tweet_id))
tweets_MX<- tweets_MX %>% distinct(tweet_id, .keep_all = TRUE)

#Keep only tweet_id and translate text
#MX_tweets_sent %>% select(tweet_id, clean_text_en) %>% 
 # iconv(clean_text_en, to = "ASCII", sub=" ")
text <- iconv(tweets_MX$clean_text_en, to = "ASCII", sub = " ")

tweets_MX <- tweets_MX %>% mutate(document=row_number()) 

text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)  # Remove the "RT" (retweet) and usernames 
text = gsub("http.+ |http.+$", " ", text)  # Remove html links
text = gsub("http[[:alnum:]]*", "", text) 
#text = gsub("[[:punct:]]", " ", text)  # Remove punctuation
text = gsub("[ |\t]{2,}", " ", text)  # Remove tabs
text = gsub("^ ", "", text)  # Leading blanks
text = gsub(" $", "", text)  # Lagging blanks
text = gsub(" +", " ", text) # General spaces 



text = tolower(text)
#tweets = unique(tweets) I will not remove duplicated. I count retwees as individual tweets
#Should I lemmatize?


library(tm)
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, removePunctuation, 
                 preserve_intra_word_dashes = TRUE,
                 preserve_intra_word_contractions = TRUE)
corpus <- tm_map(corpus, removeNumbers)
#writeLines(as.character(corpus[[1500]]))
#Re run translation without removing accounts

corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("migrantcaravan","caravana", 
                                       "migrante", "caravanamigrante", 
                                       "caravanademigrantes",
                                       "caravanamigrant",
                                       "caravanamigr", 
                                       "migrant",
                                       "migrantcaravan", "caravan",
                                       "migrantcaravana", "anonym", "amp"))
library(wordcloud)
set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
wordcloud(corpus, min.freq = 50, scale = c(4, 0.2) , random.order = TRUE, col = palet)

#Convert corpus to Document Term Matrix (DTM)
dtm<-DocumentTermMatrix(corpus)
dtm

doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm

#We are looking at the words
library(dplyr)
freq = colSums(as.matrix(dtm))
length(freq)

ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]
#Deshacerme de migrantcaravana y anonym

findAssocs(dtm, "state",0.2)

library(topicmodels)
# #LDA model with 5 topics selected
# lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
#             control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                            thin = 500, burnin = 4000, iter = 2000))
# 
# #LDA model with 2 topics selected
# lda_2 = LDA(dtm, k = 2, method = 'Gibbs', 
#             control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                            thin = 500, burnin = 4000, iter = 2000))
# 
# #LDA model with 10 topics selected
# lda_10 = LDA(dtm, k = 10, method = 'Gibbs', 
#              control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                             thin = 500, burnin = 4000, iter = 2000))
# #Top 10 terms or words under each topic
# top10terms_5 = as.matrix(terms(lda_5,10))
# top10terms_2 = as.matrix(terms(lda_2,10))
# top10terms_10 = as.matrix(terms(lda_10,10))
# 
# top10terms_5
# top10terms_2
# top10terms_10

#Find number of topics with code from FCOROWE
# install.packages("quanteda")
# library(quanteda)
# corpus2<-unlist(corpus)
# corpus3<-as.data.frame(corpus2)
# corpus4<-corpus(corpus3, text_field = "corpus2")
# rm(corpus3)
# dfm <- dfm(corpus4)
# rm(corpus)

install.packages("ldatuning")
library(ldatuning)

# result <- FindTopicsNumber(
#   dtm,
#   topics = seq(from = 2, to = 20, by = 1), # Select range of topics to classify
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), # Select metrics
#   method = "Gibbs", # Gibbs sampling
#   control = list(seed = 281190), # Set seed so can replicate
#   verbose = TRUE
# )
#code from https://content-analysis-with-r.com/6-topic_models.html

ldatuning.metrics <- FindTopicsNumber(dtm, 
                                      topics = seq(from = 2, to = 15, by = 1), 
                                      metrics = c("Griffiths2004", "CaoJuan2009", 
                                                  "Arun2010", "Deveaud2014"), 
                                      method = "Gibbs", 
                                      control = list(seed = 77), 
                                      mc.cores = 2L, 
                                      verbose = TRUE)

# ldatuning.metrics20 <- FindTopicsNumber(dtm, 
#                                       topics = seq(from = 2, to = 22, by = 2), 
#                                       metrics = c("Griffiths2004", "CaoJuan2009", 
#                                                   "Arun2010", "Deveaud2014"), 
#                                       method = "Gibbs", 
#                                       control = list(seed = 77), 
#                                       mc.cores = 2L, 
#                                       verbose = TRUE)
# 
# ldatuning.metrics50 <- FindTopicsNumber(dtm, 
#                                         topics = seq(from = 2, to = 48, by = 4), 
#                                         metrics = c("Griffiths2004", "CaoJuan2009", 
#                                                     "Arun2010", "Deveaud2014"), 
#                                         method = "Gibbs", 
#                                         control = list(seed = 77), 
#                                         mc.cores = 2L, 
#                                         verbose = TRUE)

FindTopicsNumber_plot(ldatuning.metrics)
FindTopicsNumber_plot(ldatuning.metrics20)
FindTopicsNumber_plot(ldatuning.metrics50)



# Classify tweets
# lda_result9<-LDA(dtm, k = 9, method = "Gibbs",
#                  control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                                 thin = 500, burnin = 4000, iter = 2000))
lda_result4 <- LDA(dtm, k = 4, method = "Gibbs",
                    control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                    thin = 500, burnin = 4000, iter = 2000))
# lda_result8<-LDA(dtm, k = 8, method = "Gibbs",
#                  control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                                 thin = 500, burnin = 4000, iter = 2000))
install.packages("remotes")
remotes::install_github("doug-friedman/topicdoc")

library(topicdoc)
diag_df_4 <- topic_diagnostics(lda_result4, dtm)
diag_df_4

# diag_df_9 <- topic_diagnostics(lda_result9, dtm)
# diag_df_9

diag_df_8 <- topic_diagnostics(lda_result8, dtm)
diag_df_8
library(ggplot2)
library(dplyr, warn.conflicts = F)
library(tidyr)
library(stringr)

# diag_df_10 <- diag_df_10 %>%
#   mutate(topic_label = terms(lda_result10, 5) %>%
#            apply(2, paste, collapse = ", "),
#          topic_label = paste(topic_num, topic_label, sep = " - "))
# 
# diag_df_10 %>% 
#   gather(diagnostic, value, -topic_label, -topic_num) %>%
#   ggplot(aes(x = topic_num, y = value,
#              fill = str_wrap(topic_label, 25))) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~diagnostic, scales = "free") +
#   labs(x = "Topic Number", y = "Diagnostic Value",
#        fill = "Topic Label", title = "All Topic Model Diagnostics")

diag_df_4 <- diag_df_4 %>%
  mutate(topic_label = terms(lda_result4, 5) %>%
           apply(2, paste, collapse = ", "),
         topic_label = paste(topic_num, topic_label, sep = " - "))

diag_df_4 %>% 
  gather(diagnostic, value, -topic_label, -topic_num) %>%
  ggplot(aes(x = topic_num, y = value,
             fill = str_wrap(topic_label, 25))) +
  geom_bar(stat = "identity") +
  facet_wrap(~diagnostic, scales = "free") +
  labs(x = "Topic Number", y = "Diagnostic Value",
       fill = "Topic Label", title = "All Topic Model Diagnostics")

# diag_df_8 <- diag_df_8 %>%
#   mutate(topic_label = terms(lda_result8, 5) %>%
#            apply(2, paste, collapse = ", "),
#          topic_label = paste(topic_num, topic_label, sep = " - "))
# 
# diag_df_8 %>% 
#   gather(diagnostic, value, -topic_label, -topic_num) %>%
#   ggplot(aes(x = topic_num, y = value,
#              fill = str_wrap(topic_label, 25))) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~diagnostic, scales = "free") +
#   labs(x = "Topic Number", y = "Diagnostic Value",
#        fill = "Topic Label", title = "All Topic Model Diagnostics")

terms(lda_result4,10)
#terms(lda_result10,10)
terms(lda_result8,10)
#topic_coherence(lda_results, AssociatedPress[1:20,])

#install.packages("tidyr")
library(tidyr)
library(tidytext)

#Save outputs
# Get probability of each word being associated with each topic
lda_topics4 <- tidy(lda_result4, matrix = "beta")
# Get probability of each tweet belonging to each topic
lda_docs4 <- tidy(lda_result4, matrix = "gamma") %>%
  arrange(document, desc(gamma))

# Get probability of each word being associated with each topic
lda_topics4 <- tidy(lda_result4, matrix = "beta")
# Get probability of each tweet belonging to each topic
lda_docs4 <- tidy(lda_result4, matrix = "gamma") %>%
  arrange(document, desc(gamma))

# Get probability of each word being associated with each topic
#lda_topics7 <- tidy(lda_result7, matrix = "beta")
#Get probability of each tweet belonging to each topic
#lda_docs7 <- tidy(lda_result7, matrix = "gamma") %>%
  arrange(document, desc(gamma))

as.data.frame(t(terms(lda_result4,10))) %>% stargazer(summary = FALSE)

# top_terms8 <- 
#   lda_topics8 %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# 
# top_terms8 %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic,nrow=3, scales = "free") +
#   coord_flip()


top_terms4 <- 
  lda_topics4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms4 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic,nrow=3, scales = "free") +
  coord_flip()

# lda_agg13 <- lda_docs13 %>% 
#   group_by(document) %>% 
#   filter(gamma == max(gamma))


# lda_agg8 <- lda_docs8 %>% 
#   group_by(document) %>% 
#   filter(gamma == max(gamma))

lda_agg4 <- lda_docs4 %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma))

text1 <- as.data.frame(text)
text1 <- text1 %>% mutate(document = rownames(text1))

terms(lda_result4,10)
lda_agg4 %>% filter(topic == 1) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 2) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 3) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 4) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% sample_n(10)

# lda_agg8 %>% filter(topic == 5) %>% 
#   left_join(text1, by="document")  %>% as.data.frame() %>% sample_n(10)
# 
# lda_agg8 %>% filter(topic == 6) %>% 
#   left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

# lda_agg8 %>% filter(topic == 7) %>% 
#   left_join(text1, by="document")  %>% as.data.frame() %>% sample_n(10)
# 
# lda_agg8 %>% filter(topic == 8) %>% 
#   left_join(text1, by="document")  %>% as.data.frame() %>% sample_n(10)

library(stargazer)
as.data.frame(t(terms(lda_result8,10))) %>% stargazer(summary = FALSE)


tweets_MX <- tweets_MX %>% mutate(document=row_number()) 

docs<-tweets_MX %>% select(tweet_id, clean_text_en, document, `vader_MX$compound`) %>% 
  mutate(document=as.character(document)) %>% 
  rename(vader= `vader_MX$compound`)

docs_topic<-docs %>% right_join(lda_agg4, by = "document")

docs_topic %>% ggplot( aes(x=as.factor(topic), y=vader)) + 
  geom_boxplot()+
  xlab("Topic")+
  ylab("Sentiment")
table(docs_topic$topic)

#table(docs_topic$topic)
#1   2   3   4   5   6   7   8 
#663 686 719 598 710 726 740 646 


docs_topic %>%
  mutate(topic = as.factor(topic)) %>%
  mutate(sentiment=case_when(vader > 0 ~ "Positive",
                             vader < 0 ~ "Negative")) %>% 
  mutate(vader2=abs(vader)) %>% 
  filter(vader2>0.05) %>% 
  ggplot( aes(x=vader2, fill=sentiment)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap( ~ topic)


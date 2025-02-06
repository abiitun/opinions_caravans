#Topic modelling for the US

tweets_US<-tweets_US %>% distinct(tweet_id, .keep_all = TRUE)

text <- iconv(tweets_US$clean_text_en, to = "ASCII", sub = " ")

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
writeLines(as.character(corpus[[1500]]))
#Re run translation without removing accounts

corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("migrantcaravan","caravana", 
                                        "migrante", "caravanamigrante", 
                                        "caravanademigrantes",
                                        "caravanamigrant",
                                        "caravanamigr", 
                                        "migrant",
                                        "migrantcaravan", "caravan",
                                        "migrantcaravana", "anonym", 
                                        "amp"))
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
#remove de migrantcaravana y anonym

findAssocs(dtm, "state",0.2)
library(topicmodels)


install.packages("ldatuning")
library(ldatuning)


ldatuning.metrics <- FindTopicsNumber(dtm, 
                                      topics = seq(from = 2, to = 15, by = 1), 
                                      metrics = c("Griffiths2004", "CaoJuan2009", 
                                                  "Arun2010", "Deveaud2014"), 
                                      method = "Gibbs", 
                                      control = list(seed = 77), 
                                      mc.cores = 2L, 
                                      verbose = TRUE)

# ldatuning.metrics20 <- FindTopicsNumber(dtm, 
#                                         topics = seq(from = 2, to = 22, by = 2), 
#                                         metrics = c("Griffiths2004", "CaoJuan2009", 
#                                                     "Arun2010", "Deveaud2014"), 
#                                         method = "Gibbs", 
#                                         control = list(seed = 77), 
#                                         mc.cores = 2L, 
#                                         verbose = TRUE)
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
#FindTopicsNumber_plot(ldatuning.metrics20)
#FindTopicsNumber_plot(ldatuning.metrics50)


lda_result5<-LDA(dtm, k = 5, method = "Gibbs",
                 control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                                thin = 500, burnin = 4000, iter = 2000))
#lda_result10 <- LDA(dtm, k = 10, method = "Gibbs",
#                    control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
#                                   thin = 500, burnin = 4000, iter = 2000))
lda_result4 <- LDA(dtm, k = 4, method = "Gibbs",
                    control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                                   thin = 500, burnin = 4000, iter = 2000))
lda_result3 <- LDA(dtm, k = 3, method = "Gibbs",
                   control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                                  thin = 500, burnin = 4000, iter = 2000))
lda_result6 <- LDA(dtm, k = 6, method = "Gibbs",
                   control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                                  thin = 500, burnin = 4000, iter = 2000))

terms(lda_result5,10)
terms(lda_result4,10)

terms(lda_result10,10)
#as.data.frame(t(terms(lda_result10,10))) %>% stargazer(summary = FALSE)
as.data.frame(t(terms(lda_result5,10))) %>% stargazer(summary = FALSE)
as.data.frame(t(terms(lda_result4,10))) %>% stargazer(summary = FALSE)


install.packages("remotes")
remotes::install_github("doug-friedman/topicdoc")

library(topicdoc)
diag_df_6 <- topic_diagnostics(lda_result6, dtm)
diag_df_6

diag_df_5 <- topic_diagnostics(lda_result5, dtm)
diag_df_5

diag_df_4 <- topic_diagnostics(lda_result4, dtm)
diag_df_4

diag_df_3 <- topic_diagnostics(lda_result3, dtm)
diag_df_3

terms(lda_result6,10)
terms(lda_result5,10)
terms(lda_result4,10)
terms(lda_result3,10)



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

diag_df_4 <- diag_df_4 %>%
  mutate(topic_label = terms(lda_result5, 5) %>%
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


library(tidyr)
library(tidytext)

#Save outputs
# Get probability of each word being associated with each topic
#lda_topics5 <- tidy(lda_result5, matrix = "beta")
# Get probability of each tweet belonging to each topic
#lda_docs5 <- tidy(lda_result5, matrix = "gamma") %>%
#  arrange(document, desc(gamma))
#########Look at distribution of vader per topic

lda_topics4 <- tidy(lda_result4, matrix = "beta")
# Get probability of each tweet belonging to each topic
lda_docs4 <- tidy(lda_result4, matrix = "gamma") %>%
  arrange(document, desc(gamma))

lda_agg4 <- lda_docs4 %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma))

# lda_topics3 <- tidy(lda_result4, matrix = "beta")
# # Get probability of each tweet belonging to each topic
# lda_docs4 <- tidy(lda_result4, matrix = "gamma") %>%
#   arrange(document, desc(gamma))
# 
# lda_agg4 <- lda_docs4 %>% 
#   group_by(document) %>% 
#   filter(gamma == max(gamma))
# lda_topics3 <- tidy(lda_result3, matrix = "beta")
# # Get probability of each tweet belonging to each topic
# lda_docs3 <- tidy(lda_result3, matrix = "gamma") %>%
#   arrange(document, desc(gamma))
# 
# lda_agg4 <- lda_docs4 %>% 
#   group_by(document) %>% 
#   filter(gamma == max(gamma))

tweets_US <- tweets_US %>% mutate(document=row_number()) 
docs<-tweets_US %>% select(tweet_id, clean_text_en, document, `vader_US$compound`) %>% 
  mutate(document=as.character(document)) %>% 
  rename(vader= `vader_US$compound`)

docs_topic<-docs %>% right_join(lda_agg4, by = "document")

docs_topic %>% ggplot( aes(x=as.factor(topic), y=vader)) + 
  geom_boxplot()
table(docs_topic$topic)

docs_topic %>%
  mutate(topic = as.factor(topic)) %>%
  ggplot( aes(x=vader)) +
  geom_histogram() +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap( ~ topic)



####Finding what the topics are about


text1 <- as.data.frame(text)
text1 <- text1 %>% mutate(document = rownames(text1))

terms(lda_result3,10)
terms(lda_result4,10)
terms(lda_result5,10)

lda_agg4 %>% filter(topic == 1) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 2) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 3) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% slice_sample(n=10)

lda_agg4 %>% filter(topic == 4) %>% 
  left_join(text1, by="document")  %>% as.data.frame() %>% sample_n(10)



as.data.frame(t(terms(lda_result4,10))) %>% stargazer(summary = FALSE)

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


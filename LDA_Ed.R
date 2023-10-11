library(tidyverse)
library(tidytext)
library(topicmodels)


source("sona-first-steps.R")
source("DataClean.R")



uni <- data_count %>%
  unnest_tokens(word, speech, token = "words") %>%
  filter(!word %in% stop_words$word)

uni_tdf <- uni %>%
  group_by(ids,word) %>%
  count() %>%  
  ungroup() 

dtm_uni <- uni_tdf %>% 
  cast_dtm(ids, word, n)


uni_lda <- LDA(dtm_uni, k = 2, control = list(seed = 1234))
uni_lda



term <- as.character(uni_lda@terms)
topic1 <- uni_lda@beta[1,]
topic2 <- uni_lda@beta[2,]
reviews_topics <- tibble(term = term, topic1 = topic1, topic2 = topic2)

reviews_topics <- reviews_topics %>% 
  pivot_longer(c(topic1, topic2), names_to = 'topic', values_to = 'beta') %>%
  mutate(beta = exp(beta)) # pr(topic k generates word i) = exp(beta_ik)
head(reviews_topics)

uni_topics <- tidy(uni_lda, matrix = 'beta')
head(uni_topics)



top_terms <- uni_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip()


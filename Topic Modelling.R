library(tidyverse)
library(tidytext)
library(topicmodels)


sona$speech = as.character(sona$speech)
sona$speechID = 1:nrow(sona)

speeches = data.frame(speech = as.character(sona$speech), speechID = sona$speechID)

tidy_speeches <- speeches %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)

speeches_tdf <- tidy_speeches %>%
  group_by(speechID,word) %>%
  count() %>%  
  ungroup()

dtm_speeches <- speeches_tdf %>% 
  cast_dtm(speechID, word, n)

speeches_lda <- LDA(dtm_speeches, k = 2, control = list(seed = 1234))
speeches_lda
str(speeches_lda)

term <- as.character(speeches_lda@terms)
topic1 <- speeches_lda@beta[1,]
topic2 <- speeches_lda@beta[2,]
speeches_topics <- tibble(term = term, topic1 = topic1, topic2 = topic2)

speeches_topics <- speeches_topics %>% 
  pivot_longer(c(topic1, topic2), names_to = 'topic', values_to = 'beta') %>%
  mutate(beta = exp(beta)) # pr(topic k generates word i) = exp(beta_ik)
head(speeches_topics)

speeches_topics <- tidy(speeches_lda, matrix = 'beta')
head(speeches_topics)

top_terms <- speeches_topics %>%
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


beta_spread <- speeches_topics %>%
  mutate(topic = paste0('topic', topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = 'Log2 ratio of beta in topic 2 / topic 1') +
  coord_flip()


speeches_gamma <- speeches %>% 
  left_join(tidy(speeches_lda, matrix = 'gamma') %>% 
              mutate(speechID = as.numeric(document)) %>%       
              select(-document) %>%
              spread(key = topic, value = gamma, sep = '_'))

## LOST IT from here on
speeches_gamma %>% group_by(speechID) %>% summarize(ntopic1 = sum(topic_1 > 0.5))


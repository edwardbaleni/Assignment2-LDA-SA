rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)

### Load Data

load("SonaData.RData")

### Separate speeches into sentences and sentences into words

unnest_reg = "[^\\w_#@']"

speechSentences = as_tibble(sona) %>%
  mutate(speechID = 1:36) %>%
  rename(president = president_13) %>%
  unnest_tokens(sentences, speech, token = "sentences") %>%
  select(speechID, president, year, sentences) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "’", "'")) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "'", "")) %>%
  mutate(sentences, sentences = str_remove_all(sentences, "[0-9]")) %>%
  mutate(sentID = row_number())

wordsWithSentID = speechSentences %>% 
  unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(sentID, speechID, president, year, word)

### Pre-Processing
### Counting the number of times each word appears in the speeches of each president

library(topicmodels)

speechTDF = wordsWithSentID %>%
  group_by(president, word) %>%
  count() %>%  
  ungroup() 

dtmSpeech = speechTDF %>% 
  cast_dtm(president, word, n)

kSeq = 2:4
topicsList = list()

for (k in kSeq){
  
  speechLDA = LDA(dtmSpeech, k = k, control = list(seed = 2023))
  
  speechTopics = tidy(speechLDA, matrix = 'beta')
  
  topicsList[[k-1]] = speechTopics
  
  speechTopics %>%
    group_by(topic) %>%
    slice_max(n = 15, order_by = beta) %>% 
    ungroup() %>%
    arrange(topic, -beta) %>%
    ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = 'free') + 
    coord_flip() + xlab(" ") +
    theme_bw(base_size = 12)
}

names(topicsList) = c("Two", "Three", "Four")

chosenTopic = topicsList$Two

chosenTopic %>%
  group_by(topic) %>%
  slice_max(n = 15, order_by = beta) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + 
  coord_flip() + xlab(" ") +
  theme_bw(base_size = 12)

removeWords = chosenTopic %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10) 

aaa = chosenTopic %>%
  filter(!term %in% removeWords$term) 

aaa %>%
  group_by(topic) %>%
  slice_max(n = 10, order_by = beta) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + 
  coord_flip() + xlab(" ") +
  theme_bw(base_size = 12)

# compare the top 15 most used words in each topic 
# to the top 15 most commonly used words for each president

wordsWithSentID %>%
  group_by(president) %>%
  count(word) %>%
  slice_max(n = 15, order_by = n) %>% 
  ungroup() %>%
  arrange(president, -n) %>%
  ggplot(aes(reorder(word, n), n, fill = factor(president))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, scales = 'free') + 
  coord_flip() + xlab(" ") +
  theme_bw(base_size = 12)

# compare terms with the greatest difference in Betas between topics

wideBeta = function(speechTopics, k, thresh = 0.001){
  
  if (k == 2){
    
    betaWide = speechTopics %>%
      mutate(topic = paste0("topic", topic)) %>%
      pivot_wider(names_from = topic, values_from = beta) %>% 
      filter(topic1 > thresh | topic2 > thresh) %>%
      mutate(log_ratio1.2 = log2(topic2 / topic1))
  }
  
  if (k == 3){
    
    betaWide = speechTopics %>%
      mutate(topic = paste0("topic", topic)) %>%
      pivot_wider(names_from = topic, values_from = beta) %>% 
      filter(topic1 > thresh | topic2 > thresh | topic3 > thresh) %>%
      mutate(log_ratio1.2 = log2(topic2 / topic1)) %>%
      mutate(log_ratio1.3 = log2(topic3 / topic1)) %>%
      mutate(log_ratio2.3 = log2(topic3 / topic2))
  }
  
  if (k == 4){
    
    betaWide = speechTopics %>%
      mutate(topic = paste0("topic", topic)) %>%
      pivot_wider(names_from = topic, values_from = beta) %>% 
      filter(topic1 > thresh | topic2 > thresh | topic3 > thresh | topic4 > thresh ) %>%
      mutate(log_ratio1.2 = log2(topic2 / topic1)) %>%
      mutate(log_ratio1.3 = log2(topic3 / topic1)) %>%
      mutate(log_ratio1.4 = log2(topic4 / topic1)) %>%
      mutate(log_ratio2.3 = log2(topic3 / topic2)) %>%
      mutate(log_ratio2.4 = log2(topic4 / topic2)) %>%
      mutate(log_ratio3.4 = log2(topic4 / topic3)) 
  }
  
  return(betaWide)
}

betaWide = wideBeta(chosenTopic, k = 2, thresh = 0.001)

betaWide %>%
  select(term, log_ratio1.2) %>%
  filter(abs(log_ratio1.2) >= 5) %>%
  ggplot(aes(reorder(term, log_ratio1.2), log_ratio1.2)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 2 and 1") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.3) %>%
  filter(abs(log_ratio1.3) >= 100) %>%
  ggplot(aes(reorder(term, log_ratio1.3), log_ratio1.3)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 3 and 1") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.4) %>%
  filter(abs(log_ratio1.4) >= 10) %>%
  ggplot(aes(reorder(term, log_ratio1.4), log_ratio1.4)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 4 and 1") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.5) %>%
  filter(abs(log_ratio1.5) >= 10) %>%
  ggplot(aes(reorder(term, log_ratio1.5), log_ratio1.5)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 5 and 1") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.6) %>%
  filter(abs(log_ratio1.6) >= 100) %>%
  ggplot(aes(reorder(term, log_ratio1.6), log_ratio1.6)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 6 and 1") +
  theme_bw(base_size = 12)


### Document-Topic Probabilities

speechTDF_DTP = wordsWithSentID %>%
  group_by(speechID, word) %>%
  count() %>%  
  ungroup() 

dtmSpeech_DTP = speechTDF_DTP %>% 
  cast_dtm(speechID, word, n)

speechLDAforDTP = LDA(dtmSpeech_DTP, k = 6, control = list(seed = 2023))

gamma = tidy(speechLDAforDTP, matrix = 'gamma') 
gamma$gamma = round(gamma$gamma, 3)

speechGamma = left_join(speechSentences %>% 
                        mutate(speechID = as.character(speechID)) %>%
                        select(-sentences, -sentID), 
                        gamma,
                        by = c("speechID" = "document"), 
                        relationship = "many-to-many")


## Number of Mandela's sentences that LDA estimated to not be from Mandela
# I don't know what's going on here 

speechGamma %>% 
  group_by(speechID) %>%
  # summarise_all(speechID = first(speechID)) %>%
  filter(president == "Mandela") %>%
  filter(topic != 4 & gamma > 0.5) 

speechGamma %>% 
  filter(president == "deKlerk") %>%
  filter(topic != 1 & gamma > 0.5)

speechGamma %>% 
  filter(president == "Mbeki") %>%
  filter(topic != 1 & gamma > 0.5) 

speechGamma %>% 
  filter(president == "Motlanthe") %>%
  filter(topic != 2 & gamma > 0.5) 

speechGamma %>% 
  filter(president == "Ramaphosa") %>%
  filter(topic != 6 & gamma > 0.5) 

speechGamma %>% 
  filter(president == "Zuma") %>%
  filter(topic != 3 & gamma > 0.5) 

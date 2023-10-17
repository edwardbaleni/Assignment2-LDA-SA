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

chosenTopic = topicsList$Four

# keeping the term in the topic with the highest beta value

chosenTopic %>%
  group_by(term) %>%
  slice(which.max(beta)) %>%
  ungroup() %>%
  group_by(topic) %>%
  slice_max(n = 10, order_by = beta) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + 
  coord_flip() + xlab(" ") +
  theme_bw(base_size = 12)

# compare terms with the greatest difference in Betas between topics
# plots are for all comparisons, assess and visualise for importance

wideBeta = function(speechTopics, k, thresh){
  
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

betaWide = wideBeta(chosenTopic, k = 4, thresh = 0.001)

betaWide %>%
  select(term, log_ratio1.2) %>%
  filter(abs(log_ratio1.2) >= 10) %>%
  mutate(pos = log_ratio1.2 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio1.2), log_ratio1.2, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 2 and 1") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.3) %>%
  filter(abs(log_ratio1.3) >= 50) %>%
  mutate(pos = log_ratio1.3 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio1.3), log_ratio1.3, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 3 and 1") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio1.4) %>%
  filter(abs(log_ratio1.4) >= 200) %>%
  mutate(pos = log_ratio1.4 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio1.4), log_ratio1.4, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 4 and 1") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio2.3) %>%
  filter(abs(log_ratio2.3) >= 10) %>%
  mutate(pos = log_ratio2.3 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio2.3), log_ratio2.3, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 3 and 2") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio2.4) %>%
  filter(abs(log_ratio2.4) >= 10) %>%
  mutate(pos = log_ratio2.4 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio2.4), log_ratio2.4, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 4 and 2") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

betaWide %>%
  select(term, log_ratio3.4) %>%
  filter(abs(log_ratio3.4) >= 100) %>%
  mutate(pos = log_ratio3.4 >= 0) %>%
  ggplot(aes(reorder(term, log_ratio3.4), log_ratio3.4, fill = pos)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Beta Log Ratio between Topic 4 and 3") +
  scale_fill_manual(values=c("red", "blue")) +
  guides(fill = "none") +
  theme_bw(base_size = 12)

### Document-Topic Probabilities

speechTDF_DTP = wordsWithSentID %>%
  group_by(speechID, word) %>%
  count() %>%  
  ungroup()

dtmSpeech_DTP = speechTDF_DTP %>% 
  cast_dtm(speechID, word, n)

speechLDAforDTP = LDA(dtmSpeech_DTP, k = 4, control = list(seed = 2023))

gamma = tidy(speechLDAforDTP, matrix = 'gamma')
# gamma$gamma = round(gamma$gamma, 3)

speechGamma = left_join(speechSentences %>% 
                        mutate(speechID = as.character(speechID)) %>%
                        select(-sentences, -sentID), 
                        gamma,
                        by = c("speechID" = "document"),
                        relationship = "many-to-many") %>%
  group_by(speechID) %>%
  slice_head(n = 4) %>%
  ungroup() %>%
  mutate(gamma = round(gamma, 3))

manData = speechGamma %>% 
  filter(president == "Mandela") %>% arrange(topic)

manPlotData = data.frame("Man.Len" = 1:length(unique(manData$speechID)),
                         "Man.Gam.Top1" = manData$gamma[1:7],
                         "Man.Gam.Top2" = manData$gamma[8:14],
                         "Man.Gam.Top3" = manData$gamma[15:21],
                         "Man.Gam.Top4" = manData$gamma[22:28])

ggplot(manPlotData, aes(x = as.factor(Man.Len))) +
  geom_point(aes(y = Man.Gam.Top1), col = "black", size = 4) +
  geom_point(aes(y = Man.Gam.Top2), col = "deeppink", size = 4) +
  geom_point(aes(y = Man.Gam.Top3), col = "deepskyblue", size = 4) +
  geom_point(aes(y = Man.Gam.Top4), col = "darkorchid", size = 4) +
  geom_line(aes(x = Man.Len, y = Man.Gam.Top1), col = "black", linewidth = 2) +
  geom_line(aes(x = Man.Len, y = Man.Gam.Top2), col = "deeppink", linewidth = 2) +
  geom_line(aes(x = Man.Len, y = Man.Gam.Top3), col = "deepskyblue", linewidth = 2) +
  geom_line(aes(x = Man.Len, y = Man.Gam.Top4), col = "darkorchid", linewidth = 2) +
  ylab("Speech Topic Probability") + xlab("Speech") +
  theme_bw(base_size = 12)

mbeData = speechGamma %>% 
  filter(president == "Mbeki") %>% 
  mutate(speechID = as.integer(speechID)) %>%
  arrange(topic, speechID)

mbePlotData = data.frame("Mbe.Len" = 1:length(unique(mbeData$speechID)),
                         "Mbe.Gam.Top1" = mbeData$gamma[1:10],
                         "Mbe.Gam.Top2" = mbeData$gamma[11:20],
                         "Mbe.Gam.Top3" = mbeData$gamma[21:30],
                         "Mbe.Gam.Top4" = mbeData$gamma[31:40])

ggplot(mbePlotData, aes(x = as.factor(Mbe.Len))) +
  geom_point(aes(y = Mbe.Gam.Top1), col = "black", size = 4) +
  geom_point(aes(y = Mbe.Gam.Top2), col = "deeppink", size = 4) +
  geom_point(aes(y = Mbe.Gam.Top3), col = "deepskyblue", size = 4) +
  geom_point(aes(y = Mbe.Gam.Top4), col = "darkorchid", size = 4) +
  geom_line(aes(x = Mbe.Len, y = Mbe.Gam.Top1), col = "black", linewidth = 2) +
  geom_line(aes(x = Mbe.Len, y = Mbe.Gam.Top2), col = "deeppink", linewidth = 2) +
  geom_line(aes(x = Mbe.Len, y = Mbe.Gam.Top3), col = "deepskyblue", linewidth = 2) +
  geom_line(aes(x = Mbe.Len, y = Mbe.Gam.Top4), col = "darkorchid", linewidth = 2) +
  ylab("Speech Topic Probability") + xlab("Speech") +
  theme_bw(base_size = 12)

zumData = speechGamma %>% 
  filter(president == "Zuma") %>% 
  mutate(speechID = as.integer(speechID)) %>%
  arrange(topic, speechID)

zumPlotData = data.frame("Zum.Len" = 1:length(unique(zumData$speechID)),
                         "Zum.Gam.Top1" = zumData$gamma[1:10],
                         "Zum.Gam.Top2" = zumData$gamma[11:20],
                         "Zum.Gam.Top3" = zumData$gamma[21:30],
                         "Zum.Gam.Top4" = zumData$gamma[31:40])

ggplot(zumPlotData, aes(x = as.factor(Zum.Len))) +
  geom_point(aes(y = Zum.Gam.Top1), col = "black", size = 4) +
  geom_point(aes(y = Zum.Gam.Top2), col = "deeppink", size = 4) +
  geom_point(aes(y = Zum.Gam.Top3), col = "deepskyblue", size = 4) +
  geom_point(aes(y = Zum.Gam.Top4), col = "darkorchid", size = 4) +
  geom_line(aes(x = Zum.Len, y = Zum.Gam.Top1), col = "black", linewidth = 2) +
  geom_line(aes(x = Zum.Len, y = Zum.Gam.Top2), col = "deeppink", linewidth = 2) +
  geom_line(aes(x = Zum.Len, y = Zum.Gam.Top3), col = "deepskyblue", linewidth = 2) +
  geom_line(aes(x = Zum.Len, y = Zum.Gam.Top4), col = "darkorchid", linewidth = 2) +
  ylab("Speech Topic Probability") + xlab("Speech") +
  theme_bw(base_size = 12)

ramData = speechGamma %>% 
  filter(president == "Ramaphosa") %>% 
  mutate(speechID = as.integer(speechID)) %>%
  arrange(topic, speechID)

ramPlotData = data.frame("Ram.Len" = 1:length(unique(ramData$speechID)),
                         "Ram.Gam.Top1" = ramData$gamma[1:7],
                         "Ram.Gam.Top2" = ramData$gamma[8:14],
                         "Ram.Gam.Top3" = ramData$gamma[15:21],
                         "Ram.Gam.Top4" = ramData$gamma[22:28])

ggplot(ramPlotData, aes(x = as.factor(Ram.Len))) +
  geom_point(aes(y = Ram.Gam.Top1), col = "black", size = 4) +
  geom_point(aes(y = Ram.Gam.Top2), col = "deeppink", size = 4) +
  geom_point(aes(y = Ram.Gam.Top3), col = "deepskyblue", size = 4) +
  geom_point(aes(y = Ram.Gam.Top4), col = "darkorchid", size = 4) +
  geom_line(aes(x = Ram.Len, y = Ram.Gam.Top1), col = "black", linewidth = 2) +
  geom_line(aes(x = Ram.Len, y = Ram.Gam.Top2), col = "deeppink", linewidth = 2) +
  geom_line(aes(x = Ram.Len, y = Ram.Gam.Top3), col = "deepskyblue", linewidth = 2) +
  geom_line(aes(x = Ram.Len, y = Ram.Gam.Top4), col = "darkorchid", linewidth = 2) +
  ylab("Speech Topic Probability") + xlab("Speech") +
  theme_bw(base_size = 12)

# includes the sentences with the gammas

sentencesGamma = left_join(speechSentences %>% 
                             mutate(speechID = as.character(speechID)), 
                           gamma,
                           by = c("speechID" = "document"),
                           relationship = "many-to-many") %>%
  mutate(gamma = round(gamma, 3))

sentencesGamma %>%
  filter(grepl("economy", sentences))

chosenTopic %>%
  group_by(term) %>%
  slice(which.max(beta)) %>%
  ungroup() %>%
  group_by(topic) %>%
  slice_max(n = 10, order_by = beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)

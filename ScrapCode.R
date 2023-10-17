
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

betaWide = speechTopics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001 | topic6 > .001) %>%
  mutate(log_ratio1.2 = log2(topic2 / topic1)) %>%
  mutate(log_ratio1.3 = log2(topic3 / topic1)) %>%
  mutate(log_ratio1.4 = log2(topic4 / topic1)) %>%
  mutate(log_ratio1.5 = log2(topic5 / topic1)) %>%
  mutate(log_ratio1.6 = log2(topic6 / topic1)) %>%
  mutate(log_ratio2.3 = log2(topic3 / topic2)) %>%
  mutate(log_ratio2.4 = log2(topic4 / topic2)) %>%
  mutate(log_ratio2.5 = log2(topic5 / topic2)) %>%
  mutate(log_ratio2.6 = log2(topic6 / topic2)) %>%
  mutate(log_ratio3.4 = log2(topic4 / topic3)) %>%
  mutate(log_ratio3.5 = log2(topic5 / topic3)) %>%
  mutate(log_ratio3.6 = log2(topic6 / topic3)) %>%
  mutate(log_ratio4.5 = log2(topic5 / topic4)) %>%
  mutate(log_ratio4.6 = log2(topic6 / topic4)) %>%
  mutate(log_ratio5.6 = log2(topic6 / topic5)) 


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
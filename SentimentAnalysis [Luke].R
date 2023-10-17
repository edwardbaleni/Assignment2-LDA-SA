
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)

### Load Data

load("SonaData.RData")
load("dsfi-lexicons.Rdata")

### Separate speeches into sentences and sentences into words

unnest_reg = "[^\\w_#@']"

speechSentences = as_tibble(sona) %>%
  rename(president = president_13) %>%
  unnest_tokens(sentences, speech, token = "sentences") %>%
  select(president, year, sentences) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "’", "'")) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "'", "")) %>%
  mutate(sentences, sentences = str_remove_all(sentences, "[0-9]")) %>%
  mutate(sentID = row_number())

wordsWithSentID = speechSentences %>% 
  unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(sentID, president, year, word)

### Join with Sentiment Lexicon

wordsSentiment = wordsWithSentID %>% 
  left_join(bing, by = "word") %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), "neutral", bing_sentiment))

head(wordsSentiment)
table(wordsSentiment$bing_sentiment)

### Most frequent Words
## Positive

## How often each president has said the top 10 most frequent positive words

wordsSentiment %>%
  filter(bing_sentiment == "positive") %>%
  count(president, word) %>%
  group_by(president) %>% 
  filter(rank(desc(n)) <= 10) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col() + 
  facet_wrap(~president, scales = "free_y") + coord_flip() + xlab(" ")

## Each Presidents Top 15 Most Frequent Positive words
## Top 5 is highlighted

wordsSentiment %>%
  filter(president == "Mandela") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 6)

wordsSentiment %>%
  filter(president == "Mbeki") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Motlanthe") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  #filter(id <= 15) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speech") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Ramaphosa") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  #filter(id <= 15) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Zuma") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "deKlerk") %>%
  filter(bing_sentiment == "positive") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "purple", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speech") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

## Each Presidents Most Frequent Negative words
## Top 5 is highlighted

wordsSentiment %>%
  filter(president == "Mandela") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Mbeki") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Motlanthe") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speech") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Ramaphosa") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "Zuma") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

wordsSentiment %>%
  filter(president == "deKlerk") %>%
  filter(bing_sentiment == "negative") %>%
  count(word) %>%
  #filter(rank(desc(n)) <= 20) %>%
  arrange(desc(n)) %>%
  mutate(id = 1:dim(.)[1]) %>%
  filter(id <= 15) %>%
  ggplot(aes(reorder(word, n), n)) + geom_col(fill = "orange", col = "black") + 
  coord_flip() + 
  xlab(" ") + ylab("Times Used in Speech") +
  theme_bw(base_size = 12) +
  gghighlight(id <= 5)

### Count the positive and negative sentiments in each speech 

wordsSentiment %>%
  group_by(year, president) %>%
  filter(bing_sentiment == "positive") %>%
  count(bing_sentiment) %>%
  ggplot(aes(x = year, y = n, shape = president)) + 
  geom_point(col = "purple", size = 5, stroke = 2) +
  xlab("Year") + ylab("Number of Positive Sentiments in Speech") +
  theme_bw(base_size = 12) + 
  scale_x_discrete(name = "Year", 
                   breaks = c("1994","1999","2004", "2009", 
                              "2014", "2019", "2023")) +
  scale_shape_manual(values = c(5, 15, 1, 18, 0, 16))

wordsSentiment %>%
  group_by(year, president) %>%
  filter(bing_sentiment == "negative") %>%
  count(bing_sentiment) %>%
  ggplot(aes(x = year, y = n, shape = president)) + 
  geom_point(col = "orange", size = 5, stroke = 2) +
  xlab("Year") + ylab("Number of Negative Sentiments in Speech") + 
  theme_bw(base_size = 12) + 
  scale_x_discrete(name = "Year", 
                   breaks = c("1994","1999","2004", "2009", 
                              "2014", "2019", "2023")) +
  scale_shape_manual(values = c(5, 15, 1, 18, 0, 16)) 

## Net Sentiment of Speech

wordsSentiment %>%
  group_by(year, president, bing_sentiment) %>%
  filter(bing_sentiment == "negative" | bing_sentiment == "positive") %>%
  count(bing_sentiment) %>%
  ungroup(bing_sentiment) %>%
  mutate(netSent = n - first(n)) %>%
  filter(bing_sentiment == "positive") %>%
  ggplot(aes(x = year, y = netSent, shape = president)) + 
  geom_point(col = "red", size = 5, stroke = 2) +
  xlab("Year") + ylab("Number of Net Positive Sentiments in Speech") + 
  theme_bw(base_size = 12) + 
  scale_x_discrete(name = "Year", 
                   breaks = c("1994","1999","2004", "2009", 
                              "2014", "2019", "2023")) +
  scale_shape_manual(values = c(5, 15, 1, 18, 0, 16)) 

## Change in Net Positive Sentiment between first and last speech

wordsSentiment %>%
  group_by(year, president, bing_sentiment) %>%
  filter(bing_sentiment == "negative" | bing_sentiment == "positive") %>%
  count(bing_sentiment) %>%
  ungroup(bing_sentiment) %>%
  mutate(netSent = n - first(n)) %>%
  filter(bing_sentiment == "positive") %>%
  ungroup(year) %>%
  mutate(changeNetSent = last(netSent) - netSent) %>% 
  filter(row_number() == 1) %>%
  ggplot(aes(x = as.factor(president), y = changeNetSent)) + 
  geom_bar(fill = "red", stat = "identity") +
  xlab("President") + ylab("Change in Net Positive Sentiments in Speech") + 
  theme_bw(base_size = 12) 

### NRC Sentiment Analysis

wordsSentimentNRC = wordsSentiment %>% 
  left_join(nrc, by = "word", relationship = "many-to-many") %>% 
  rename(nrc_sentiment = sentiment)

head(wordsSentimentNRC)

wordsSentimentNRC %>%
  add_count(president, name = "n_words") %>%
  na.omit() %>%
  group_by(president, nrc_sentiment) %>%
  summarize(prop = n() / first(n_words)) %>% 
  ungroup() %>%
  group_by(president, nrc_sentiment) %>%
  summarize(mean_prop = mean(prop)) %>% 
  ungroup() %>%
  group_by(president) %>%
  mutate(nrc_id = 1:10) %>%
  ungroup() %>%
  filter(nrc_id <= 5) %>%
  rename(President = president) %>%
  ggplot(aes(reorder(nrc_sentiment, mean_prop), mean_prop, fill = President)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Average Proportion") +
  theme_bw(base_size = 12)

wordsSentimentNRC %>%
  add_count(president, name = "n_words") %>%
  na.omit() %>%
  group_by(president, nrc_sentiment) %>%
  summarize(prop = n() / first(n_words)) %>% 
  ungroup() %>%
  group_by(president, nrc_sentiment) %>%
  summarize(mean_prop = mean(prop)) %>% 
  ungroup() %>%
  group_by(president) %>%
  mutate(nrc_id = 1:10) %>%
  ungroup() %>%
  filter(nrc_id >= 6) %>%
  rename(President = president) %>%
  ggplot(aes(reorder(nrc_sentiment, mean_prop), mean_prop, fill = President)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + xlab("") + ylab("Average Proportion") +
  theme_bw(base_size = 12)

### Word Clouds (maybe for EDA...)

library(wordcloud)

set.seed(2023)

wordsWithSentID %>%
  anti_join(stop_words) %>%
  filter(president == "Mandela") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 25, random.order = TRUE, scale = c(2.5, 1)))

wordsWithSentID %>%
  anti_join(stop_words) %>%
  filter(president == "Mbeki") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 25, scale = c(2.5, 1)))

wordsWithSentID %>%
  anti_join(stop_words) %>%
  filter(president == "Ramaphosa") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 25, scale = c(2.5, 1)))

wordsWithSentID %>%
  anti_join(stop_words) %>%
  filter(president == "Zuma") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 25, scale = c(2.5, 1)))

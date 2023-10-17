##### Bigrams with Luke's Data

### Libraries

library(textdata) 
library(stringr)
library(lubridate)

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)
library(ggpubr)

### Load Data

load("SonaData.RData")
load("dsfi-lexicons.Rdata")

### Change format of date
sona$date = as.Date(sona$date, tryFormats = "%Y-%m-%d")

### Separate speeches into sentences and sentences into words

unnest_reg = "[^\\w_#@']"

speechSentences = as_tibble(sona) %>%
  rename(president = president_13) %>%
  unnest_tokens(sentences, speech, token = "sentences") %>%
  select(president, year, sentences, date) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "’", "'")) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "'", "")) %>%
  mutate(sentences, sentences = str_remove_all(sentences, "[0-9]")) %>%
  mutate(sentID = row_number())

wordsWithSentID = speechSentences %>% 
  unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(sentID, president, year, word)


# Bigrams

# Negation

# Bigram
bigram <- speechSentences %>% 
  unnest_tokens(word, sentences, token = 'ngrams', n = 2)

# Collect some negation words from qdap dictionary
negation_words <- qdapDictionaries::negation.words
stop_words <- stop_words %>%
  filter(!word %in% negation_words)

# Separate bigram to use stop words
bigrams_separated <- bigram %>%
  separate(word, c("word1", "word2"), sep = " ")

# Filter out stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Unite bigram
bigrams_unite <- bigrams_filtered %>%
  unite(word1, word2)

# Obtain the negated words
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  arrange(desc(n))

# Plot of the negated words
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = word1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, ncol = 2, scales = "free_y")


# Sentiment of a bigram
# reverse the sentiment of word2 whenever it is preceded by a negation word, and then add up the number of positive and negative words within a bigram and take the difference.

bigrams_separated <- bigrams_filtered %>% 
  # add sentiment for word 1
  left_join(bing, by = c(word1 = 'word')) %>%
  rename(sentiment1 = sentiment) %>%
  mutate(sentiment1 = ifelse(is.na(sentiment1), 'neutral', sentiment1)) %>%
  # add sentiment for word 2
  left_join(bing, by = c(word2 = 'word')) %>%
  rename(sentiment2 = sentiment) %>%
  mutate(sentiment2 = ifelse(is.na(sentiment2), 'neutral', sentiment2)) %>%
  select(word1, word2, sentiment1, sentiment2, everything())

bigrams_separated

bigrams_separated <- bigrams_separated %>%
  # create a variable that is the opposite of sentiment2
  mutate(opp_sentiment2 = recode(sentiment2, 'positive' = 'negative',
                                 'negative' = 'positive',
                                 'neutral' = 'neutral')) %>%
  # reverse sentiment2 if word1 is a negation word
  mutate(sentiment2 = ifelse(word1 %in% negation_words, opp_sentiment2, sentiment2)) %>%
  # remove the opposite sentiment variable, which we don't need any more
  select(-opp_sentiment2)




bigrams_separated <- bigrams_separated %>%
  mutate(net_sentiment = (sentiment1 == 'positive') + (sentiment2 == 'positive') - 
           (sentiment1 == 'negative') - (sentiment2 == 'negative')) %>%
  unite(bigram, word1, word2, sep = ' ', remove = FALSE)
bigrams_separated %>% select(word1, word2, sentiment1, sentiment2, net_sentiment)

# Positive bigrams
bigrams_separated %>%
  filter(net_sentiment > 0) %>% # get positive bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')

# Negative biagrams
bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')

# Negated bigrams
bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  filter(word1 %in% negation_words) %>% # get bigrams where first word is negation
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')









##### Using old data, we have some analysis


#data_count$date = as.Date(data_count$date, tryFormats = "%d-%m-%Y")
data_count <-  speechSentences
nrc <- nrc %>%
  distinct(word, .keep_all = T)

# Tokenize
uni <- data_count %>%
  unnest_tokens(word, sentences, token = "words") %>%
  filter(!word %in% stop_words$word)

# Combining sentiment dictionaries with tokens
uni <- uni %>%
  left_join(bing) %>%
  rename( bing_sentiment = sentiment) %>%
  left_join(afinn) %>%
  rename(afinn_sentiment = value) %>%
  left_join(nrc) %>%
  rename(nrc_sentiment = sentiment)

# Sentiment over time
sentiment_time <- uni %>%
  group_by(date, bing_sentiment) %>%
  summarize(n = n()) 

# Positive and negative sentiment over time.
ggplot(filter(sentiment_time, bing_sentiment != 'neutral'), aes(x = date, y = n, fill = bing_sentiment)) +
  geom_col() 

sentiment_time <- sentiment_time %>% 
  left_join(sentiment_time %>% 
              group_by(date) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total) 

# A line graph depicting the rise and fall of possitive and negatice sentiment 
# over time
sentiment_time %>% filter(bing_sentiment != 'neutral') %>%
  ggplot(aes(x = date, y = freq, colour = bing_sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = bing_sentiment))

# This line gives the sentiment of the statements over sentences
sentiment_over_sentence <- uni %>%
  group_by(ids) %>%
  summarize(net_sentiment = (sum(bing_sentiment == 'positive') - sum(bing_sentiment == 'negative')),
            month = first(date))

# Negative sentiment
sentences %>% 
  left_join(sentiment_over_sentence) %>% 
  arrange(net_sentiment) %>% 
  head(10) %>%
  select(speech, net_sentiment) 

# Positive Sentiment
sentences %>% 
  left_join(sentiment_over_sentence) %>% 
  arrange(desc(net_sentiment)) %>% 
  head(10) %>%
  select(speech, net_sentiment) 


# illustrates the sentiment over time and through their texts
# If this was split up per speech it would show the change in sentiment throught the speech
# This will look at the sentiment over sections of each presidents speeches going 20
# sentences at a time
number_of_sentences <- 20
sentiment_president <- uni %>%
  count(president, index = ids %/% number_of_sentences, bing_sentiment) %>%
  pivot_wider(names_from = bing_sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(sentiment_president, aes(index, sentiment, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~president, ncol = 2, scales = "free_x")




# Word Cloud

library(wordcloud)
uni %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.order = F, min.freq = 10),  scale = c(1,.1))

library(reshape2)

uni %>%
  filter(!is.na(bing_sentiment)) %>%
  count(word, bing_sentiment, sort = TRUE) %>%
  acast(word ~ bing_sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 100, scale = c(4,.1))

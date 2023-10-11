
library(tidyverse)
library(tidytext)
library(textdata) 
library(stringr)
library(lubridate)

source("sona-first-steps.R")
source("DataClean.R")

data_count$date = as.Date(data_count$date, tryFormats = "%d-%m-%Y")

afinn <- get_sentiments('afinn') 
bing <- get_sentiments('bing') 
nrc <- get_sentiments('nrc') %>%
  distinct(word, .keep_all = T)

uni <- data_count %>%
  unnest_tokens(word, speech, token = "words") %>%
  filter(!word %in% stop_words$word)

uni <- uni %>%
  left_join(bing) %>%
  rename( bing_sentiment = sentiment) %>%
  left_join(afinn) %>%
  rename(afinn_sentiment = value) %>%
  left_join(nrc) %>%
  rename(nrc_sentiment = sentiment)

# Sentiment over time


sentiments_per_month <- uni %>%
  group_by(date, bing_sentiment) %>%
  summarize(n = n()) 

ggplot(filter(sentiments_per_month, bing_sentiment != 'neutral'), aes(x = date, y = n, fill = bing_sentiment)) +
  geom_col() 


sentiments_per_month <- sentiments_per_month %>% 
  left_join(sentiments_per_month %>% 
              group_by(date) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total) 

sentiments_per_month %>% filter(bing_sentiment != 'neutral') %>%
  ggplot(aes(x = date, y = freq, colour = bing_sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = bing_sentiment))




sentiments_per_tweet <- uni %>%
  group_by(ids) %>%
  summarize(net_sentiment = (sum(bing_sentiment == 'positive') - sum(bing_sentiment == 'negative')),
            month = first(date))

sentences %>% 
  left_join(sentiments_per_tweet) %>% 
  arrange(net_sentiment) %>% 
  head(10) %>%
  select(speech, net_sentiment) 

sentences %>% 
  left_join(sentiments_per_tweet) %>% 
  arrange(desc(net_sentiment)) %>% 
  head(10) %>%
  select(speech, net_sentiment) 


# Plot does not run
sentiments_per_tweet %>%
  group_by(month) %>%
  summarize(prop_neg = sum(net_sentiment < 0) / n()) %>%
  ggplot(aes(x = month, y = prop_neg)) +
  geom_line() + geom_smooth()





ggplot(uni, aes(ids, afinn_sentiment, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~president, ncol = 2, scales = "free_x")



number_of_sentences <- 50
jane_austen_sentiment <- uni %>%
  count(president, index = ids %/% number_of_sentences, bing_sentiment) %>%
  pivot_wider(names_from = bing_sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~president, ncol = 2, scales = "free_x")





# Most common words

bing_word_counts <- uni %>%
  count(word, bing_sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(bing_sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = bing_sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~bing_sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


library(wordcloud)

uni %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.order = F, min.freq = 10))

library(reshape2)

uni %>%
  filter(!is.na(bing_sentiment)) %>%
  count(word, bing_sentiment, sort = TRUE) %>%
  acast(word ~ bing_sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)















# Negation

# Bigram
bigram <- data_count %>% 
  unnest_tokens(word, speech, token = 'ngrams', n = 2)

bigrams_separated <- bigram %>%
  separate(word, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_unite <- bigrams_filtered %>%
  unite(word1, word2)



negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  arrange(desc(n))

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = word1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, ncol = 2, scales = "free_y")


# Sentiment of a bigram
# reverse the sentiment of word2 whenever it is preceded by a negation word, and then add up the number of positive and negative words within a bigram and take the difference.

bigrams_separated <- bigrams_separated %>% 
  # add sentiment for word 1
  left_join(bing, by = c(word1 = 'word')) %>%
  rename(sentiment1 = sentiment) %>%
  mutate(sentiment1 = ifelse(is.na(sentiment1), 'neutral', sentiment1)) %>%
  # add sentiment for word 2
  left_join(bing, by = c(word2 = 'word')) %>%
  rename(sentiment2 = sentiment) %>%
  mutate(sentiment2 = ifelse(is.na(sentiment2), 'neutral', sentiment2)) %>%
  select(date, word1, word2, sentiment1, sentiment2, everything())

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


bigrams_separated %>%
  filter(net_sentiment > 0) %>% # get positive bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')


bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')

bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  filter(word1 %in% negation_words) %>% # get bigrams where first word is negation
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab('')

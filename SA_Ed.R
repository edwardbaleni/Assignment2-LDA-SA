
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
nrc <- get_sentiments('nrc') 

uni <- data_count %>%
  unnest_tokens(word, speech, token = "words") %>%
  filter(!word %in% stop_words$word)

uni <- uni %>%
  left_join(bing) %>%
  rename( bing_sentiment = sentiment) %>%
  left_join(afinn) %>%
  rename(afinn_sentiment = value)

# nrc by far has the greatest number of sentiments, so try incorporate that at some point



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

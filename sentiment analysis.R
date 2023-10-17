## Exploring sentiment analysis

data_count

words = unnest_tokens()

sona_sentences = unnest_tokens(sona, sentence, speech, token="sentences") %>%
  mutate(sentence_id = row_number()) %>% 
  rename(president_name = president)

sona_words = sona_sentences %>% 
  unnest_tokens(word, sentence, token = "words") %>%
  select(president_name, sentence_id, word)

## Need to still get rid of numbers and do some more cleaning

all_words = sona_words %>%
  group_by(word) %>%
  count() %>%
  ungroup()

all_words_cleaned = all_words %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(str_length(word) > 2)

stopwords = stop_words$word
all_words_final = anti_join(all_words_cleaned, stop_words, by = "word")

load("dsfi-lexicons.Rdata")
afinn %>% head(10)

all_words_join_lex = all_words_final %>%
                      left_join(bing) %>%
                      select(word, sentiment, everything()) %>%
                      mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment))

positive_words = all_words_join_lex %>%
        filter(sentiment == 'positive') %>%
        select(word, n) %>%
        arrange(desc(n)) %>%
        filter(rank(desc(n)) <=20)

positive_words %>% ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab('')

negative_words = all_words_join_lex %>%
  filter(sentiment == 'negative') %>%
  select(word, n) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <=20)

negative_words %>% ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab('')

## Changes over time 

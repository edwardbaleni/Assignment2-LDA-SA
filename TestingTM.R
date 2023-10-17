
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

# Assuming 'dtm' is your document-term matrix
lda_model <- LDA(dtmSpeech, k = 5)  # Try different values of k (number of topics)

# Visual inspection
topics(lda_model)

top_terms <- terms(lda_model, 10)  # 10 represents the number of top terms to display per topic

# Print the top terms for each topic
for (i in 1:(lda_model@k)) {
  cat("Topic", i, ":", paste(top_terms[, i], collapse = ", "), "\n")
}

top_terms <- terms(lda_model, 10)  # 10 represents the number of top terms to display per topic

# Convert the top_terms matrix to a data frame for easier plotting
top_terms_df <- as.data.frame(top_terms)



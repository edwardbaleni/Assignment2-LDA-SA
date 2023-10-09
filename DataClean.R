require(dplyr)
require(stringr)
require(tidytext)
require(ggplot2)
require(tidyr)
require(forcats)
require(textstem)
require(qdap)

#sys.source("sona-first-steps.R" , envir = knitr::knit_global())

set.seed(2023)
sona <- as_tibble(sona)

# Clean data
sentences <- sona %>%
  unnest_sentences(speech, speech) %>%
  mutate(ids = row_number())

clean <- function(x){
  # remove numbers
  x <- gsub("\\d+", "", x)
  # remove contractions
  x <- gsub("/'s", "", x)
  # remove special characters
  x <- gsub("[^[:alnum:] ]", "", x)
  # lemmatization
  x <- lemmatize_strings(x)
  # remove single characters
  x <- gsub("\\s.\\s", " ", x)
}

sentences$speech <- unlist(lapply(sentences$speech, clean))

data <- sentences %>%
  filter(!president %in% c("deKlerk", "Motlanthe"))
data$president <- as.factor(data$president)

# Need to remove first sentence for each president, to analyse count well
# Since first sentence is similar for each president
hold <- c()
for (i in unique(data$filename)){
  hold <- c(hold, which( data$filename %in% i)[1])
}

data_count <- data[-hold,]
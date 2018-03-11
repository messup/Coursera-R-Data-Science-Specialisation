library(dplyr)
library(stringr)
library(tidyr)

# Week 1: Read in data, tokenize, remove profanity.

sample_fraction <- 0.90

twitter_data_complete <- data.frame(lines = readLines("en_US.twitter.txt"), stringsAsFactors = FALSE)
news_data_complete <- data.frame(lines = readLines("en_US.news.txt"), stringsAsFactors = FALSE)
blogs_data_complete <- data.frame(lines = readLines("en_US.blogs.txt"), stringsAsFactors = FALSE)

set.seed(1234)
twitter_data <- sample_frac(twitter_data_complete, sample_fraction)
news_data <- sample_frac(news_data_complete, sample_fraction)
blogs_data <- sample_frac(blogs_data_complete, sample_fraction)

data <- rbind(twitter_data, news_data, blogs_data)

tokenize <- function(sentence){
  sentence <- unlist(strsplit(sentence, "[\\.,!\\?\\:]+")) # split up sentences
  sentence <- str_to_lower(sentence) # convert to lower case
  sentence <- str_replace_all(sentence, "[^a-z ']", "")  # keep only letters, spaces, and apostrophes
  sentence <- str_replace_all(sentence, "'(?![a-z])|(?<![a-z])'", "")  # remove apostrophe if it doesn't have a letter on each side
  sentence <- str_trim(sentence, side = 'both')  # remove leading and trailing whitespace
  words = str_split(sentence, ' +')
  return(words)
}

is_not_empty <- function(x){
  if('' %in% x){
    return(FALSE)
  }
  return(TRUE)
}

tokenized <- tokenize(data$lines)
selection <- sapply(tokenized, is_not_empty)
tokenized <- tokenized[selection]

count_combinations <- function(sentence, n = 2){
  if (length(sentence) < n){
    return(0)
  }
  return(length(sentence) - n + 1)
}

# n-grams
# strategy:
# loop over each sentence
# add each combination to a vector
# then collapse into a summary table
get_n_grams <- function(input_data, n=2){
  total_combinations <- sum(sapply(input_data, count_combinations, n = n))
  j = 0
  n_grams <- character(length = total_combinations)
  for (sentence in input_data){
    if (length(sentence) >= n){
      n_combinations <- count_combinations(sentence, n)
      for (i in 1:n_combinations){
        j = j + 1
        n_grams[j] <- paste(sentence[i: (i + n - 1)], collapse = ' ')
      }
    }
  }
  n_gram_df <- as.data.frame(table(n_grams), stringsAsFactors = FALSE)
  return(n_gram_df)
}

get_word_frequency <- function(data){
  word <- unlist(data)
  word_df <- as.data.frame(table(word), stringsAsFactors = FALSE)
  return(word_df)
}

# only take the top 3 occurences for each unique sequence
reduce_table_size <- function(ngramtable, n){
  if(n == 1){
    stop("reduce_table_size not intended for unigrams, stopping...")
  }
  cols_to_merge <- c('w_1', 'w_2', 'w_3')[(5 - n):3]
  ngramtable <- ngramtable %>% 
    unite(cols_to_merge, col = 'key', sep = ' ', remove = FALSE) %>% 
    arrange(key, -Freq)
  keys <- ngramtable$key
  uniques <- unique(keys)
  i = 1  # i tracks unique key row
  j = 1  # j tracks ngram frequency table row
  nrows <- nrow(ngramtable)
  rows_to_keep <- rep(TRUE, nrows)
  while(i <= length(uniques)){
    j_initial = j
    while((keys[j] == uniques[i]) & (keys[j] == uniques[i]) & j < nrows){
      j <- j + 1
    }
    N <- j - j_initial
    if(N > 3){
      for(k in (j_initial + 3):(j - 1)){
        rows_to_keep[k] <- FALSE
      }
    }
    i <- i + 1
  }
  ngramtable <- select(ngramtable, -key)
  print(nrow(ngramtable))
  ngramtable <- ngramtable[rows_to_keep,]
  print(nrow(ngramtable))
  return(ngramtable)
}

# faster to do unigrams this way.
unigrams <- tokenized %>% 
  get_word_frequency() %>% 
  arrange(-Freq) %>% 
  mutate(cumulative = cumsum(Freq) / sum(Freq)) %>%
  filter(cumulative < 0.95) %>%
  select(-cumulative) %>%
  rename(w_4 = word)
write.csv(unigrams, '1grams.csv', row.names = FALSE)
dictionary <- unigrams$w_4
gc()

replace_unknown_word <- function(charvector){
  rows_to_keep <- rep(TRUE, length(charvector))
  for(i in 1:length(charvector)){
    if(!(charvector[i] %in% dictionary)){
      charvector[i] <- '<U>'
    }
  }
  return(charvector)
}

tokenized <- sapply(tokenized, replace_unknown_word)
new_columns = c('w_4', 'w_3', 'w_2', 'w_1')
for(n in 2:4){
  tokenized %>% 
    get_n_grams(n) %>% 
    arrange(-Freq) %>%
    filter(Freq > 1) %>% 
    separate(n_grams, into = new_columns[n:1], sep = ' ', remove = FALSE) %>% 
    select(-n_grams) %>%
    # mutate_at(vars(starts_with('w_')), sapply, FUN=replace_unknown_word) %>%
    filter(w_4 != '<U>') %>%
    reduce_table_size(n) %>% 
    write.csv(paste(n, 'grams.csv', sep=''), row.names = FALSE)
  gc()
}

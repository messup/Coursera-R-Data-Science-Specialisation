library(dplyr)
library(stringr)
library(tidyr)

tokenize <- function(sentence){
  sentence <- unlist(strsplit(sentence, "[\\.,!\\?\\:]+")) # split up sentences
  # sentence <- str_replace(sentence, '.+\\.', '')
  sentence <- tail(sentence, -1)  # only take the last sentence
  sentence <- str_to_lower(sentence) # convert to lower case
  sentence <- str_replace_all(sentence, "[^a-z ']", "")  # keep only letters, spaces, and apostrophes
  sentence <- str_replace_all(sentence, "'(?![a-z])|(?<![a-z])'", "")  # remove apostrophe if it doesn't have a letter on each side
  sentence <- str_trim(sentence, side='both')  # remove leading and trailing whitespace
  words = str_split(sentence, ' +')
  return(words)
}

add_discount_column <- function(df, rmin=2, rmax=10){
  df$d <- 1
  for(r in rmin:rmax){
    currN = nrow(filter(df, Freq == r))
    nextN = nrow(filter(df, Freq == r + 1))
    d_new <- ((r + 1) / r) * (nextN / currN)
    df <- mutate(df, d = if_else(Freq == r, d_new, d))
  }
  return(df)
}

get_probability <- function(word, n = 4, a = 1, prediction_tables, alpha){
  total_F <- sum(prediction_tables[[n]]$Freq)
  if(word %in% prediction_tables[[n]]$w_4){
    row <- filter(prediction_tables[[n]], w_4 == word)
    probability <- a * row$d[1] * row$Freq[1] / total_F
    return(probability)
  }
  else{
    a <- a * alpha[n]
    get_probability(word, n - 1, a, prediction_tables, alpha = alpha)
  }
}

unigrams <- read.csv('1grams.csv', stringsAsFactors = FALSE) %>% add_discount_column()
bigrams <- read.csv('2grams.csv', stringsAsFactors = FALSE) %>% add_discount_column()
trigrams <- read.csv('3grams.csv', stringsAsFactors = FALSE) %>% add_discount_column()
fourgrams <- read.csv('4grams.csv', stringsAsFactors = FALSE) %>% add_discount_column()

dictionary <- unigrams$w_4
replace_unknown_word <- function(myword){
  if(myword %in% dictionary){
    return(myword)
  }
  return('<U>')
}

predict_next_word <- function(string, verbose = FALSE){
  tokens <- rev(rev(tokenize(string))[[1]])
  tokens <- sapply(tokens, replace_unknown_word)
  w3 <- tokens[1]
  w2 <- tokens[2]
  w1 <- tokens[3]
  
  fourpred <- fourgrams %>% filter(w_1==w1, w_2==w2, w_3==w3)
  threepred <- trigrams %>% filter(w_2==w2, w_3==w3) %>% head(100)
  twopred <- bigrams %>% filter(w_3==w3) %>% head(100)
  onepred <- head(unigrams, 3)
  
  candidate_words <- unique(c(fourpred$w_4, threepred$w_4, twopred$w_4, onepred$w_4))
  
  probabilities <- numeric(length = length(candidate_words))
  
  prediction_tables <- list(onepred, twopred, threepred, fourpred)
  beta <- numeric(length = 4)
  alpha <- numeric(length = 4)
  
  for(i in 4:1){
    if(nrow(prediction_tables[[i]]) == 0){
      beta[i] <- 1
    }
    else{
      beta[i] <- 1 - sum(prediction_tables[[i]]$Freq * prediction_tables[[i]]$d) / sum(prediction_tables[[i]]$Freq)
    }
  }
  for(i in 4:2){
    if(nrow(prediction_tables[[i - 1]]) == 0){
      alpha[i] <- 1
    }
    else{
      alpha[i] <- beta[i] / sum(prediction_tables[[i - 1]]$Freq * prediction_tables[[i - 1]]$d)
    }
  }
  
  candidate_words <- candidate_words
  probabilities <- sapply(candidate_words, FUN = get_probability, prediction_tables = prediction_tables, alpha = alpha)
  
  predictions <- data.frame(next_word = candidate_words, P = probabilities, stringsAsFactors = FALSE) %>% 
    arrange(-P)
  head(predictions)

  if (verbose){
    print(paste("This many candidates:", length(candidate_words)))
    print(head(predictions))
  }
  
  # method1
  # return(predictions$next_word[1])
  
  # method2
  # return(list(word = predictions$next_word[1], score = predictions$P[1]))
  
  #method3
  return(word = predictions$next_word[1:3])
}

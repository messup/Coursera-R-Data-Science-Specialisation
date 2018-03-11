---
title: "Milestone Report"
author: "Alex Van Russelt"
date: "11 February 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---



## Introduction
This document aims to describe a dataset (or "corpus") that could be used to build a predictive model for text.

## Preparing the data
### Reading data

The provided datasets were imported and 10% of the lines were randomly sampled.


```r
sample_fraction <- 0.1

twitter_data_complete <- data.frame(lines=readLines("en_US.twitter.txt"), stringsAsFactors=FALSE)
news_data_complete <- data.frame(lines=readLines("en_US.news.txt"), stringsAsFactors=FALSE)
blogs_data_complete <- data.frame(lines=readLines("en_US.blogs.txt"), stringsAsFactors=FALSE)

set.seed(1234)
twitter_data <- sample_frac(twitter_data_complete, sample_fraction)
news_data <- sample_frac(news_data_complete, sample_fraction)
blogs_data <- sample_frac(blogs_data_complete, sample_fraction)

data <- rbind(twitter_data, news_data, blogs_data)
nrow(data)
```

```
## [1] 333670
```

333,670 sample texts were imported.

### Tokenization
Tokenization is the process of splitting a sentence into smaller parts. The chosen process was:

1. Seperate sentences by splitting on`.`, `,`, `?` and `!`.
1. Convert all characters to lower case.
1. Remove all characters except letters, spaces, and apostrophes.
1. Remove apostrophes that don't have a letter on either side (so that tokens such as `don't` are retained, but other applications are removed).
1. Remove leading and trailing whitespace.
1. Seperate words by splitting on spaces.


```r
tokenize <- function(sentence){
  sentence <- unlist(strsplit(sentence, "[\\.,!\\?\\:]+"))
  sentence <- tolower(sentence)
  sentence <- str_replace_all(sentence, "[^'|[:alpha:]|[:space:]]", "")
  sentence <- str_replace_all(sentence, "'(?![:alpha:])|(?<![:alpha:])'", "")
  sentence <- str_trim(sentence, side='both')
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
length(tokenized)
```

```
## [1] 971431
```

The dataset contained almost 1 million sentences.


### Create n-grams
The strategy to create n-grams was to loop over each sentence in the dataset and store each n-gram in a vector. Then, the vector was collapsed using `table` to create a frequency table.

```r
count_combinations <- function(sentence, n=2){
  if (length(sentence) < n){
    return(0)
  }
  return(length(sentence) - n + 1)
}

get_n_grams <- function(input_data, n=2){
  total_combinations <- sum(sapply(input_data, count_combinations, n = n))
  n_grams <- character(length = total_combinations)
  j = 0
  for (sentence in input_data){
    if (length(sentence) >= n){
      n_combinations <- count_combinations(sentence, n)
      for (i in 1:n_combinations){
        j = j + 1
        n_grams[j] <- paste(sentence[i: (i + n - 1)], collapse = ' ')
      }
    }
  }
  n_gram_df <- as.data.frame(table(n_grams), stringsAsFactors=FALSE)
  return(n_gram_df)
}

bigrams <- get_n_grams(tokenized, 2)
trigrams <- get_n_grams(tokenized, 3)
```

## Exploration
### Unigrams

```r
get_word_frequency <- function(data){
  word <- unlist(data)
  word_df <- as.data.frame(table(word), stringsAsFactors=FALSE)
  return(word_df)
}

words <- get_word_frequency(tokenized)
words <- arrange(words, -Freq)
words <- mutate(words, cumulative=cumsum(Freq)/sum(Freq))

g <- ggplot(data=head(arrange(words, -Freq), 30), aes(x=factor(word, levels=rev(unique(word))), y=Freq))
g + geom_col() + coord_flip() +
  labs(title='30 most commonly used words', x='word', y='Frequency')
```

![](milestonereport_files/figure-html/wordplots-1.png)<!-- -->

```r
g <- ggplot(data=words, aes(x=1:nrow(words), y=cumulative))
g + geom_line() + geom_hline(aes(yintercept=0.9), linetype='dashed') +
  labs(title='Coverage plot', x='Number of unique words', y='Coverage')
```

![](milestonereport_files/figure-html/wordplots-2.png)<!-- -->

To cover 50% of all word instances in the language, approximately 170 unique words were required.
To cover 90%, approximately 11,000 were required (shown in the plot above).

### Bigrams

```r
bigrams <- arrange(bigrams, -Freq)
bigrams <- mutate(bigrams, cumulative=cumsum(Freq)/sum(Freq))

g <- ggplot(data=head(arrange(bigrams, -Freq), 30), aes(x=factor(n_grams, levels=rev(unique(n_grams))), y=Freq))
g + geom_col() + coord_flip() + labs(title='30 most commonly used bigrams', x='Bigram', y='Frequency')
```

![](milestonereport_files/figure-html/bigramplots-1.png)<!-- -->

```r
g <- ggplot(data=bigrams, aes(x=1:nrow(bigrams), y=cumulative))
g + geom_line() + geom_hline(aes(yintercept=0.9), linetype='dashed') +
  labs(title='Bigram coverage plot', x='Number of unique bigrams', y='Coverage')
```

![](milestonereport_files/figure-html/bigramplots-2.png)<!-- -->

Significantly more bigrams were required to give good coverage when compared to unigrams.

### Trigrams

```r
trigrams <- arrange(trigrams, -Freq)
trigrams <- mutate(trigrams, cumulative=cumsum(Freq)/sum(Freq))

g <- ggplot(data=head(arrange(trigrams, -Freq), 30),
            aes(x=factor(n_grams, levels=rev(unique(n_grams))), y=Freq))
g + geom_col() + coord_flip() + labs(title='30 most commonly used trigrams', x='Trigram', y='Frequency')
```

![](milestonereport_files/figure-html/trigramplots-1.png)<!-- -->

```r
g <- ggplot(data=trigrams, aes(x=1:nrow(trigrams), y=cumulative))
g + geom_line() + geom_hline(aes(yintercept=0.9), linetype='dashed') +
  labs(title='Trigram coverage plot', x='Number of unique trigrams', y='Coverage')
```

![](milestonereport_files/figure-html/trigramplots-2.png)<!-- -->

Significantly more bigrams were required to give good coverage when compared to unigrams and bigrams.


## Next steps
1. Build a predictive model based on the n-grams and/or other techniques, ensuring that the model does not suggest profanity.
1. Investigate how a Markov chain could be used to store the model.
1. Determine an appropriate value for n in the n-gram model.
1. Use the coverage plots above to exclude the rarest combinations and keep the model small.
1. Investigate how a back-off model could be used within the context of a prediction model.




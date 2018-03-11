library(ggplot2)

source('build_model_backoff.R')

is_not_empty <- function(x){
  if('' %in% x){
    return(FALSE)
  }
  return(TRUE)
}

sample_fraction <- 0.15

twitter_data_complete <- data.frame(lines=readLines("en_US.twitter.txt"), stringsAsFactors=FALSE)
news_data_complete <- data.frame(lines=readLines("en_US.news.txt"), stringsAsFactors=FALSE)
blogs_data_complete <- data.frame(lines=readLines("en_US.blogs.txt"), stringsAsFactors=FALSE)

set.seed(4321)
twitter_data <- sample_frac(twitter_data_complete, sample_fraction)
news_data <- sample_frac(news_data_complete, sample_fraction)
blogs_data <- sample_frac(blogs_data_complete, sample_fraction)

data <- rbind(twitter_data, news_data, blogs_data)
tokenized <- tokenize(data$lines)
selection <- sapply(tokenized, is_not_empty)
tokenized <- tokenized[selection]

collapse_tokens <- function(tokens){
  N <- length(tokens) 
  if(N == 1){
    return(NA)
  }
  return(paste(tokens[1:(N - 1)], collapse = ' '))
}

get_last_word <- function(tokens){
  N <- length(tokens)
  return(tokens[N])
}

testdata_x <- sapply(tokenized, collapse_tokens)
testdata_y <- sapply(tokenized, get_last_word)

df <- data.frame(x = testdata_x, y = testdata_y, stringsAsFactors = FALSE)
df <- drop_na(df)
df <- head(df, 10000)

# method1
# df$predictions <- sapply(df$x, predict_next_word)

# method2
# p <- sapply(df$x, predict_next_word)
# df$predictions <- as.character(p[1,])
# df$scores <- as.numeric(p[2,])
# df <- mutate(df, correct = y == predictions)
# print(table(df$correct))
# ggplot(data = df, aes(x = correct, y = scores)) + geom_jitter() + scale_y_log10()
# ggplot(data = df, aes(fill = correct, x = scores, colour = correct)) + geom_histogram()
# ggplot(data = df, aes(fill = correct, x = scores, colour = correct)) + geom_density(alpha=0.5)
# right_predictions <- filter(df, correct)$y %>% table() %>% as.data.frame(stringsAsFactors=FALSE) %>% arrange(-Freq)
# wrong_predictions <- filter(df, !correct)$y %>% table() %>% as.data.frame(stringsAsFactors=FALSE) %>% arrange(-Freq)

p <- sapply(df$x, predict_next_word)
df$predictions_1 <- as.character(p[1,])
df$predictions_2 <- as.character(p[2,])
df$predictions_3 <- as.character(p[3,])
df <- mutate(df, correct_1 = y == predictions_1,
                 correct_2 = y == predictions_2,
                 correct_3 = y == predictions_3)
print(table(df$correct_1))
print(table(df$correct_2))
print(table(df$correct_3))

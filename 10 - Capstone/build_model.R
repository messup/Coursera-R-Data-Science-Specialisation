library(dplyr)

onegrams <- read.csv('1grams.csv', stringsAsFactors = FALSE)
bigrams <- read.csv('2grams.csv', stringsAsFactors = FALSE)
trigrams <- read.csv('3grams.csv', stringsAsFactors = FALSE)
fourgrams <- read.csv('4grams.csv', stringsAsFactors = FALSE)

tokenize <- function(sentence){
  sentence <- unlist(strsplit(sentence, "[\\.,!\\?\\:]+")) # split up sentences
  sentence <- str_to_lower(sentence) # convert to lower case
  sentence <- str_replace_all(sentence, "[^a-z ']", "")  # keep only letters, spaces, and apostrophes
  sentence <- str_replace_all(sentence, "'(?![a-z])|(?<![a-z])'", "")  # remove apostrophe if it doesn't have a letter on each side
  sentence <- str_trim(sentence, side='both')  # remove leading and trailing whitespace
  words = str_split(sentence, ' +')
  return(words)
}

predict_next_word <- function(string){
  tokens <- rev(rev(tokenize(string))[[1]])
  w3 <- tokens[1]
  w2 <- tokens[2]
  w1 <- tokens[3]
  
  fourpred <- fourgrams %>%
    filter(w_1==w1, w_2==w2, w_3==w3) %>% 
    mutate(P_new4 = P / sum(P)) %>%
    rename(prediction = w_4) %>% 
    select(prediction, P_new4)
  
  threepred <- trigrams %>%
    filter(w_1==w2, w_2==w3) %>%
    mutate(P_new3 = P / sum(P)) %>% 
    rename(prediction = w_3) %>% 
    select(prediction, P_new3)
  
  twopred <- bigrams %>%
    filter(w_1==w3) %>% 
    mutate(P_new2 = P / sum(P)) %>% 
    rename(prediction = w_2) %>% 
    select(prediction, P_new2)
  
  merged <- fourpred %>% 
    full_join(threepred, by='prediction') %>%
    full_join(twopred, by='prediction') #%>% 
    #mutate(P_final = P_new4, P_new3, P_new2)# %>% 
    #arrange(-P_new2)
  # merged$P_final <- merged %>%
  #   select(P_new4, P_new3, P_new2) %>%
  #   sapply(mean, na.rm = TRUE)
  # 
  # merged <- arrange(merged, -P_final)
  # 
  # merged
  
  merged$means <- merged %>% 
    select(P_new4, P_new3, P_new2) %>%
    # apply(MARGIN = 1, FUN = mean, na.rm = TRUE)
    apply(MARGIN = 1, FUN = sum, na.rm = TRUE)
  
  merged <- merged %>% 
    arrange(-score) %>%
    select(prediction, score)
}

# fourgrams$P_new <- NA
# fourgrams_distinct <- distinct(fourgrams, w_1, w_2, w_3)
# fourgrams <- arrange(fourgrams, w_1, w_2, w_3, w_4)
# 
# i_end = 0
# print(nrow(fourgrams_distinct))
# while(i_end < nrow(fourgrams_distinct)){
#   t <- filter(fourgrams, w_1==fourgrams_distinct$w_1[i], w_2==fourgrams_distinct$w_2[i], w_3==fourgrams_distinct$w_3[i])
#   i_start <- i_end + 1
#   i_end <- i_end + nrow(t)
#   P_total <- sum(t$P)
#   fourgrams$P_new[i_start:i_end] <- fourgrams$P[i_start:i_end] / P_total
#   print(i_end)
# }
# 
# i_end = 0
# # for(j in 1:nrow(fourgrams_distinct)){
# for(j in 1:1000){
#   i_start <- i_end + 1
#   while(myfunc(fourgrams, fourgrams_distinct, i_end + 1, j)){
#     i_end <- i_end + 1
#   }
#   P_total <- sum(fourgrams$P[i_start:i_end])
#   for(k in i_start:i_end){
#     fourgrams$P_new[k] <- fourgrams$P[k] / P_total
#   }
#   # print(paste(j, i_end - i_start))
# }
# 
# myfunc <- function(df, ref_df, i, j){
#   if(all(df[i, 1:3] == ref_df[j, 1:3])){
#     return(TRUE)
#   }
#   return(FALSE)
# }
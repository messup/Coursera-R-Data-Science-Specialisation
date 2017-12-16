best <- function(state_name, outcome_name){
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv",
                colClasses='character')

  ## Check that state is valid:
  ## Select all rows that match the given state name.
  ## If the resulting dataframe is empty, assume the state is invalid.
  df <- df[df$State == state_name,]
  if (nrow(df) == 0){
    stop("invalid state")
  }
  
  ## Check that outcome is valid.
  ## First clean column names (remove full stops).
  ## Then prepare a string for what the column name should be,
  ## and clean it (convert to lowercase and remove spaces).
  ## Then check to see whether a column with that name exists.
  ## If not, stop and display an error.
  names(df) <- tolower(names(df))
  names(df) <- gsub(".", "", names(df), fixed=TRUE)
  outcome_column <- paste("Hospital 30Day Death Mortality Rates from", outcome_name)
  outcome_column <- tolower(outcome_column)
  outcome_column <- gsub(" ", "", outcome_column)
  if (!outcome_column %in% names(df)){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30day mortality rate
  df[[outcome_column]] <- as.numeric(df[[outcome_column]])
  df <- df[order(df[["hospitalname"]]),]
  df <- df[order(df[[outcome_column]]),]
  df[["hospitalname"]][1]
}

rankhospital <- function(state_name, outcome_name, num="best"){
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv",
                 colClasses='character')
  
  ## Check that state is valid:
  ## Select all rows that match the given state name.
  ## If the resulting dataframe is empty, assume the state is invalid.
  df <- df[df$State == state_name,]
  if (nrow(df) == 0){
    stop("invalid state")
  }
  
  ## Check that outcome is valid.
  ## First clean column names (remove full stops).
  ## Then prepare a string for what the column name should be,
  ## and clean it (convert to lowercase and remove spaces).
  ## Then check to see whether a column with that name exists.
  ## If not, stop and display an error.
  names(df) <- tolower(names(df))
  names(df) <- gsub(".", "", names(df), fixed=TRUE)
  outcome_column <- paste("Hospital 30Day Death Mortality Rates from", outcome_name)
  outcome_column <- tolower(outcome_column)
  outcome_column <- gsub(" ", "", outcome_column)
  if (!outcome_column %in% names(df)){
    stop("invalid outcome")
  }
  
  df[[outcome_column]] <- as.numeric(df[[outcome_column]])
  df <- df[!is.na(df[[outcome_column]]),]
  
  ## Return hospital name in that state with the given 30day mortality rate rank
  if (num == "best"){
    rank <- 1
  }
  else if (num == "worst"){
    rank <- nrow(df)
  }
  else {
    rank <- num
  }
  df <- df[order(df[["hospitalname"]]),]
  df <- df[order(df[[outcome_column]]),]
  df[["hospitalname"]][rank]
}

rankall <- function(outcome_name, num="best"){
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv",
                 colClasses='character')
  
  ## Get vector containing unique state names
  state_names <- unique(df$State)
  state_names <- state_names[order(state_names)]
  hospital_names <- character(length(state_names))
  
  ## Check that outcome is valid.
  ## First clean column names (remove full stops).
  ## Then prepare a string for what the column name should be,
  ## and clean it (convert to lowercase and remove spaces).
  ## Then check to see whether a column with that name exists.
  ## If not, stop and display an error.
  names(df) <- tolower(names(df))
  names(df) <- gsub(".", "", names(df), fixed=TRUE)
  outcome_column <- paste("Hospital 30Day Death Mortality Rates from", outcome_name)
  outcome_column <- tolower(outcome_column)
  outcome_column <- gsub(" ", "", outcome_column)
  if (!outcome_column %in% names(df)){
    stop("invalid outcome")
  }
  
  df[[outcome_column]] <- as.numeric(df[[outcome_column]])
  df <- df[!is.na(df[[outcome_column]]),]
  df <- df[order(df[["hospitalname"]]),]
  df <- df[order(df[[outcome_column]]),]
  
  if (num == "best"){
    rank <- 1
  }
  else {
    rank <- num
  }
  
  for(i in 1:length(state_names)){
    df_state <- df[df[["state"]] == state_names[i],]
    if (num == "worst"){
      rank <- nrow(df_state)
    }
    hospital_names[i] <-  df_state[["hospitalname"]][rank]
  }
  
  data.frame("hospital"=hospital_names, "state"=state_names)
}

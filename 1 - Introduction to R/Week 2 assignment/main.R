pollutantmean <- function(directory, pollutant, id = 1:332){
  means <- numeric(length(id))
  weights <- numeric(length(id))
  index <- 1
  for (sensor in id){
    filename <- paste(directory, "/", formatC(sensor, width=3, flag="0"), ".csv", sep="")
    df <- read.csv(filename)
    new_mean <- mean(df[[pollutant]], na.rm=TRUE)
    new_weight <- sum(!is.na(df[[pollutant]]))
    if (!is.na(new_mean)){
      means[index] <- new_mean
      weights[index] <- new_weight
    }
    index <- index + 1
  }
  total_mean <- sum(means * weights) / sum(weights)
  # total_mean = mean(means)
  total_mean
}

# print(pollutantmean("C:/Users/Alex/Documents/Coursera Learn R/Week 2 assignment/specdata", "nitrate"))

complete <- function(directory, id = 1:332){
  nobs <- numeric(length(id))
  index <- 1
  for (sensor in id){
    filename <- paste(directory, "/", formatC(sensor, width=3, flag="0"), ".csv", sep="")
    df <- read.csv(filename)
    nobs[index] <- nrow(df[complete.cases(df),])
    index <- index + 1
  }
  data.frame(id, nobs)
}

# set.seed(42)
# cc <- complete("C:/Users/Alex/Documents/Coursera Learn R/Week 2 assignment/specdata", 332:1)
# use <- sample(332, 10)
# print(cc[use, "nobs"])

corr <- function(directory, threshold = 0){
  df <-  complete(directory)
  df <- df[as.numeric(df[['nobs']]) > threshold,] # may have to be greater to or equal than
  id <- df[['id']]
  correlations <- numeric(length(id))
  index = 1
  for (sensor in id){
    filename <- paste(directory, "/", formatC(sensor, width=3, flag="0"), ".csv", sep="")
    df_sensor <- read.csv(filename)
    df_sensor <- df_sensor[complete.cases(df_sensor),]
    correlations[index] <- cor(df_sensor[['sulfate']], df_sensor[['nitrate']])
    index <- index + 1
  }
  correlations
}

cr <- corr("C:/Users/Alex/Documents/Coursera Learn R/Week 2 assignment/specdata", 2000)
n <- length(cr)
cr <- corr("C:/Users/Alex/Documents/Coursera Learn R/Week 2 assignment/specdata", 1000)

cr <- sort(cr)
print(c(n, round(cr, 4)))

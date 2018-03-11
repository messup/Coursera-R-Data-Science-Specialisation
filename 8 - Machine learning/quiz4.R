library(ElemStatLearn)
library(randomForest)
library(caret)

# Question 1
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
model_rf <- train(y ~ ., data = vowel.train, method='rf')
model_boost <- train(y ~ ., data = vowel.train, method='gbm')

predicted_rf <- predict(model_rf, vowel.test)
predicted_boost <- predict(model_boost, vowel.test)
message(paste("Random forest accuracy:", sum(predicted_rf == vowel.test$y) / length(predicted_rf)))
message(paste("Gradient boost accuracy:", sum(predicted_boost == vowel.test$y) / length(predicted_boost)))
message(paste("Agreement accuracy:", sum(predicted_boost == predicted_rf) / length(predicted_rf)))

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
model_rf <- train(diagnosis ~ ., data = training, method='rf')
model_boost <- train(diagnosis ~ ., data = training, method='gbm')
model_lda <- train(diagnosis ~ ., data = training, method='lda')

predicted_rf <- predict(model_rf, testing)
predicted_boost <- predict(model_boost, testing)
predicted_lda <- predict(model_lda, testing)

message(paste("Random forest accuracy:", sum(predicted_rf == testing$diagnosis) / length(predicted_rf)))
message(paste("Gradient boost accuracy:", sum(predicted_boost == testing$diagnosis) / length(predicted_boost)))
message(paste("Linear discriminant accuracy:", sum(predicted_lda == testing$diagnosis) / length(predicted_lda)))

predictions_combined <- data.frame(predicted_rf, predicted_boost, predicted_lda, diagnosis=testing$diagnosis)
model_combined <- train(diagnosis ~ ., data=predictions_combined, method='rf')

predicted_ensemble <- predict(model_combined, predictions_combined)
message(paste("Stacked model accuracy:", sum(predicted_ensemble == testing$diagnosis) / length(predicted_ensemble)))

# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
model <- train(CompressiveStrength ~ ., data=training, method='lasso')

library(elasticnet)
plot.enet(model$finalModel, use.color=TRUE, xvar="penalty")
legend("topright", model$coefnames)

# Question 4
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
model <- bats(tstrain)
predictions <- forecast(model, level=95, h=nrow(testing))
count <- sum((testing$visitsTumblr > predictions$lower) & (testing$visitsTumblr < predictions$upper))
percentage <- count / nrow(testing) * 100
message(percentage)

# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
model <- svm(CompressiveStrength ~ ., data=training)
predictions <- predict(model, testing)
message(RMSE(predictions, testing$CompressiveStrength))


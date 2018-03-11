set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method='glm', data=trainSA, family='binomial')

predicted <- predict(model, trainSA)
actual <- trainSA$chd
message(missClass(as.numeric(actual), as.numeric(predicted)))


predicted_test <- predict(model, testSA)
actual_test <- testSA$chd
message(missClass(actual_test, as.numeric(predicted_test)))

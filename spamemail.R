library(caret)
library(kernlab)
data(spam)
head(spam)
View(spam)
library(caTools)
split=sample.split(spam$type,SplitRatio = 0.8)
training=subset(spam,split==TRUE)
testing=subset(spam,split==FALSE)
modelfit=train(type~.,data=training,method="glm")
modelfit
predictions=predict(modelfit,newdata=testing)
predictions
confusionMatrix(predictions,testing$type)

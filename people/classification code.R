###script 1
setwd("C:/Users/LENOVO/Desktop/people")

testdata=read.delim("Classification1Test.txt",sep="\t")##test file

testdata$C==1##to make number of columns equal in test data set

people=read.delim("ClassificationProblem1.txt",sep="\t")##to read tab seprated file

dim(people)
## to check dimensions of the data

str(people)
##to know about the structure of the data and class of data

table(people$C)

summary(people)
# ## gives the statistical summary of the data helps to know about distributions
# and normalisationof
# the data as well as about the na's and nan's


###script2
people$F16=as.Date(people$F16,"%m/%d/%Y")
people$F15=as.Date(people$F15,"%m/%d/%Y")

people$F25=difftime(people$F16,people$F15,units="days")
##MAKING A NEW COLUMN WITH A DIFFERENCE IN DATES F16-F15
##WHICH SHOWS THE TOTAL TIME IN NUMBER OF DAyS

##removing the dates as they are non numeric 
people$F16=NULL
people$F15=NULL
people$Index=NULL
people$F25=as.numeric(people$F25)
str(people)
## AS R read index column and added an extra column


# when  training a model using different features and few of them are off the scale in their magnitude, 
# then the results might get influenced by them instead of all the relevant features.so we need to normalize
#all the values
norm=function(x)###to normalize the data
{
  return ((x-min(x))/max(x)-min(x))
}

people1=data.frame(lapply(people[1:22],norm))

people1$C=as.factor(people1$C)##to make dependent variable factor
people=people1


library(caTools)## library for splitting the dataset
split=sample.split(people$C,SplitRatio = 0.8)##dividing the dataset into train and test for the learning model
train=subset(people,split==TRUE)
test=subset(people,split==FALSE)

##model fitting using logistic regression
learningmodel=glm(C~.,family=binomial(link="logit"),data=train)
##fitting the model with train data set

summary(learningmodel)##gives us the interpreations from the model

# Now we can analyze the fitting and interpret what the model is telling us.
# features such as F1,F3,F5,F6,F7,F8,F9,F11,F12,F13,F14, are not statistically significant.
##As these variables have very high p values, F4 has the lowest p-value suggesting a 
#strong association of this feature with the probability of c. 
##so we will remove the non significant features from our model and train the model again with final features

finalmodel=glm(C~+F2+F4+F10+F12+F17+F18+F19+F20+F21+F22,family = binomial(link="logit"),data=train)

summary(finalmodel)

# Analysis of deviance
anova(finalmodel,test="Chisq")

### as the reult shows the deviance of features is mentioned according to there significance
## f12 feature has come out to be least significant and can be removed.

# McFadden R^2 to check fitting of the model
#install.packages("pscl")
library(pscl)
pR2(finalmodel) ## to check the model fit value of logistic regresssion

#predicting the results with test data set
# If prob > 0.5 then 1, else 0
testresults=predict(finalmodel,newdata =test,type='response')
testresults
testresults=ifelse(testresults<0.3,0,1)

# Confusion matrix ## to check true positive and true negtive
library(caret)
confusionMatrix(testresults,test$C)
accuracy=(9292+3322)/(1643+5979+9292+3322)
accuracy

library(InformationValue)
optimalCutoff(test$C,testresults,optimiseFor ="misclasserror",returnDiagnostics = TRUE )###to check the confusion matrix values of model

# ROC###based n test data## visualization 
library(ROCR)
predictiontest=predict(finalmodel, newdata=test, type="response")
pr=prediction(predictiontest, test$C)

# TPR = sensitivity, FPR=specificity
prf=performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


####final prediction of the classifiction test data
finalprediction=predict(finalmodel, newdata=testdata, type="response")
finalprediction=ifelse(finalprediction> 0.5,1,0)
finalprediction
write.csv(finalprediction,"finalprediction.csv")


#### i used logistic regression here because it is a binary classification and logistic regression is best for the
#solution of binary classification problems.

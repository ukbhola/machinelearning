Make sure all the categorical variables are converted into factors. 
The function rpart will run a regression tree if the response variable is numeric, and a classification tree if it is a factor.
rpart parameter - Method - "class" for a classification tree ; "anova" for a regression tree
minsplit : minimum number of observations in a node before splitting. Default value - 20
minbucket : minimum number of observations in terminal node (leaf). Default value - 7 (i.e. minsplit/3)
xval : Number of cross validations
Prediction (Scoring) : If type = "prob": This is for a classification tree. It generates probabilities - Prob(Y=0) and Prob(Y=1).
Prediction (Classification) : If type = "class": This is for a classification tree. It returns 0/1.
setwd("E:/projects/german credit card data")
##read data file
mydata=read.csv("credit.csv")
##
##check attributres of the data
str(mydata)
##check number of rows and colums 
dim(mydata)
###divide data into trainig nd test data 
dt=sort(sample(nrow(mydata),nrow(mydata)*.7))
train=mydata[dt,]
edit(train)
###decision tree model
library(rpart)
mtree=rpart(Creditability~., data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
mtree
##plot tree
plot(mtree)
text(mtree)
###beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
###view
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

###fancy plot
fancyRpartPlot(mtree)
####pruning
# Select the tree size that has least misclassification rate (prediction error).
#'CP' stands for Complexity Parameter of the tree.
# We want the cp value (with a simpler tree) that has least xerror(cross validated error).
printcp((mtree))
bestcp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]
###PRUNE THE TREE USING BEST CP
pruned=prune(mtree,cp=bestcp)
###plot the pruned tree
plot(pruned)
text(pruned)
prp(pruned,faclen =2,cex = 0.8,extra=1)
fancyRpartPlot(pruned)
##confusion matrix using training data

conf.matrix=table(train$Creditability, predict(pruned,type="class"))
conf.matrix
row.names(conf.matrix)=paste("actual",rownames(conf.matrix))
colnames(conf.matrix)=paste("predicted",colnames(conf.matrix))
print(conf.matrix)
#Scoring
library(ROCR)

val1 = predict(pruned,train,type = "prob")
#Storing Model Performance Scores
pred_val <-prediction(val1[,2],train$Creditability)
##calculating area under curve
perf_val=performance(pred_val,"auc")
perf_val
# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)
##calculating true positive and false positive rate
pred_tpr=performance(pred_val,"fpr","tpr")
























































































































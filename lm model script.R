setwd("E:/projects/walmart")
train=read.csv("train.csv")
test=read.csv("test.csv")
str(train)
summary(train)
table(is.na(train))
test$Item_Outlet_Sales=1
dim(test)
dim(train)

combi=rbind(train,test)
summary(combi)
combi$Item_Weight[is.na(combi$Item_Weight)]=median(combi$Item_Weight,na.rm = TRUE)
summary(combi$Item_Weight)
combi$Item_Visibility =ifelse(combi$Item_Visibility==0,median(combi$Item_Visibility),combi$Item_Visibility)
summary(combi$Item_Visibility)
summary(combi$Outlet_Size)
levels(combi$Outlet_Size)[1]="other"
#rename levels of Item_Fat_Content
library(plyr)
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" =                                   "Regular"))
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
combi$Year=2013 - combi$Outlet_Establishment_Year
combi
combi$Item_Fat_Content=factor(combi$Item_Fat_Content,levels = c('Regular','Low Fat'),labels=c(1,0))
library(dplyr)
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))
#divide data set
new_train=combi[1:nrow(train),]
new_test=combi[-(1:nrow(train)),]

##linear model
lmmodel=lm(log(Item_Outlet_Sales)~.,data=new_train)
summary(lmmodel)
par(mfrow=c(2,2))
plot(lmmodel)
#check rmse value
library("Metrics")
rmse(new_train$Item_Outlet_Sales,exp(lmmodel$fitted.values))
###building an decison tree model
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)
##setting tree control parameters
fitcontrol=trainControl(method = "cv",number = 5)
cartGrid=expand.grid(.cp=(1:50)*0.01)
tree_model=train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart", trControl = fitcontrol, tuneGrid = cartGrid)
print(tree_model)
main_tree=rpart(Item_Outlet_Sales ~ ., data = new_train, control = rpart.control(cp=0.01))
prp(main_tree)
pre_score=predict(main_tree, type = "vector")
rmse(new_train$Item_Outlet_Sales, pre_score)


####now predicting the finalvalue
main_predict=predict(main_tree, newdata = new_test, type = "vector")
sub_file=data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,       Item_Outlet_Sales = main_predict)
write.csv(sub_file,'Decision_tree_sales.csv')
      
setwd("E:/projects/fuel efficiency of cars")
library(ggplot2)
library(car)
library(corrplot)
library(caret)
#loading data set
data("mtcars")
str(mtcars)
head(mtcars)
summary(mtcars)
#converting variables into factorrs data prepration
mtcars$am=as.factor(mtcars$am)
mtcars$cyl=as.factor(mtcars$cyl)
mtcars$vs=as.factor(mtcars$vs)
mtcars$cyl=as.factor(mtcars$cyl)
#identifying and correcting collineraty
#dropping depending vriable for correcting collinearity
mtcarsa=subset(mtcars,select = -c(mpg))
#identifying numeric data
numericdata=mtcarsa[sapply(mtcarsa,is.numeric)]
##now describe correlation
descCor=cor(numericdata)
print(descCor)
##to visualize co relation numeric data
corrplot(descCor,method ="number",type="full")
#checking variables that are highly correlated
hcor=findCorrelation(descCor,cutoff = 0.7)
hcor
hcornames=colnames(numericdata)[hcor]
###remove colnames which are highly corelated and create anew dataset
dat3=mtcars[,-which(colnames(mtcars)%in%hcornames)]
dim(dat3)
##developing ln regressio model
regressor=lm(mpg~.,dat3)
summary(regressor)
##extracting coefficients
summary(regressor)$coeff
anova(regressor)
##getting graphs
par(mfrow=c(2,2))
plot(regressor)
##checking values
AIC(regressor)
BIC(regressor)

###variable selection using aic and bic with forwared,reverse and step wise
library(MASS)
step=stepAIC(regressor,direction="both")
summary(step)
step=stepAIC(regressor,direction = "backward")
summary(step)
#Forward Selection based on AIC
step=stepAIC(regressor, direction="forward")
summary(step)
###stepwise selection with bic
n=dim(dat3)[1]
stepBIC=stepAIC(regressor,k=log(n))
summary(stepBIC)
AIC(stepBIC)
BIC(stepBIC)
##standardised coefficints
library(QuantPsyc)
print(lm.beta(stepBIC))
vif(stepBIC)

###testing other assumpions
#Autocorrelation Test
durbinWatsonTest(stepBIC)

#Normality Of Residuals (Should be > 0.05)
res=residuals(stepBIC,type="pearson")
shapiro.test(res)

#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC)

#Outliers - Bonferonni test
outlierTest(stepBIC)

#See Residuals
resid = residuals(stepBIC)

#Relative Importance
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC)

###see predicted value
#See Predicted Value
pred = predict(stepBIC,dat3)
#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))
###calculating rmse
#Calculating RMSE
rmse = sqrt(mean((dat3$mpg - pred)^2))
print(rmse)

###cross validation

library(DAAG)
kfold=cv.lm(dat3,stepBIC,,m=5)
kfold = cv.lm(data=dat3, stepBIC, m=5)
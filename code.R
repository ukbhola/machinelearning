setwd("E:/projects/walmart")
#loading the data
train=read.csv("train.csv")
test=read.csv("test.csv")
#check dimesnions
dim(train)
dim(test)
#check variables and their  types  in train
str(train)
#check missing values
is.na(train)
table(is.na(train))
#check where are the missing values
colSums(is.na(train)) #col item weight has maximum missing values
#get more infersnces from the data
summary(train)
#graphical representation using bivariate analysis
library(ggplot2)
ggplot(train,aes(x=Item_Visibility,y=Item_Outlet_Sales))+geom_point(size=2.5,color="red")+xlab("item visibility")+ylab("outletsales")+ggtitle("visibilty vs sales")
#comparing more features
ggplot(train,aes(Outlet_Identifier,Item_Outlet_Sales))+geom_bar(stat = "identity",color="blue")+theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+xlab("outlet name")+ylab("outletsales")+ggtitle("item outlet sales")

ggplot(train,aes(Item_Type,Item_Outlet_Sales))+geom_bar(stat = "identity",color="orange")+theme_bw()+theme(axis.text.x=element_text(angle=70,vjust = 0.5,color ="black" ))

ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot()+ggtitle("Box Plot")+theme(axis.text.x = element_text(angle=70,vjust=0.5))
#
#now we will combine the data sets so thatwe can impute the missing values but first we will add one column to test data set to make colms equal
dim(train)
dim(test)
#Test data set has one less column (response variable). Let's first add the column. We can give this column any value. An intuitive approach would be to extract the mean value of sales from train data set and use it as placeholder for test variable Item _Outlet_ Sales. Anyways, let's make it simple for now. I've taken a value 1. Now, we'll combine the data sets.
str(test)
test$Item_Outlet_Sales=1
str(test)
#comining both data data
dim(train)
dim(test)
combi<-rbind(train, test)
#REPLACE MISSING VALUES IN COMBI DATA WITH MEAN
combi$Item_Weight[is.na(combi$Item_Weight)]=median(combi$Item_Weight,na.rm = TRUE)
table(is.na(combi$Item_Weight))
#TAKING CARE OF CATEGORICAL AND CONTINOUS VARIABLES
combi$Item_Visibility=ifelse(combi$Item_Visibility==0,median(combi$Item_Visibility),combi$Item_Visibility)
#now proceeding to categorical variables
summary(combi)
levels(combi$Outlet_Size)[1]="other"
library(plyr)
levels(combi$Item_Fat_Content)
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content,
                               c("LF"="Low Fat","reg"="Regular"))
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content,
                               c("Low Fat"="low fat"))
levels(combi$Item_Fat_Content)
#data manipulation  feature engineering
library(dplyr)
a=combi%>%group_by(Outlet_Identifier)%>%tally()
head(a)
names(a)[2]="Outlet_Count"
#adding the outlet count to data 
combi=full_join(a,combi,by="Outlet_Identifier")
str(combi)
dim(combi)
summary(combi)
#doing same with item identifier
b=combi%>%group_by(Item_Type)%>%tally()
head(b)
names(b)[2]="item count"
combi=full_join(b,combi,by="Item_Type")
dim(combi)
#doing samae with outlet years
c=combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
head(c)
combi=full_join(c, combi)
table(combi$Item_Identifier)
# Item_Identifiers. We are about to discover a new trend. there is a pattern in the identifiers starting with "FD","DR","NC". Now, check the corresponding Item_Types to these identifiers in the data set. You'll discover, items corresponding to "DR",  are mostly eatables. Items corresponding to "FD", are drinks. And, item corresponding to "NC", are products which can't be consumed, let's call them non-consumable. Let's extract these variables into a new variable representing their counts.
q=substr(combi$Item_Identifier,1,2)
q=gsub("FD","Food",q)
q=gsub("DR","Drinks",q)
q=gsub("NC","Non-Consumable",q)
table(q)
  #now add this into our combi data
Item_Type_New=q
#hot encoding
combi$Item_Fat_Content=ifelse(combi$Item_Fat_Content == "Regular",1,0)
co
# making a linear model without data featuring

lmodel=lm(Outl)

dataset_original=read.delim("Restaurant_Reviews.tsv",quote = "",stringsAsFactors = FALSE)
##
library(tm)
library(SnowballC)
##initiating a corpus
Corpus=VCorpus(VectorSource(dataset$Review))
Corpus=tm_map(Corpus,content_transformer(tolower))
as.character(Corpus[[1]])
Corpus=tm_map(Corpus,removeNumbers)
##to check whether numbers are removed or not as.character(Corpus[[841]])
Corpus=tm_map(Corpus,removePunctuation)
Corpus=tm_map(Corpus,removeWords ,stopwords())
Corpus=tm_map(Corpus,stemDocument)
Corpus=tm_map(Corpus,stripWhitespace)
##creating the bag of words model
dtm=DocumentTermMatrix((Corpus))
dtm
dtm=removeSparseTerms(dtm,0.999)
##converting the matrix into data frame
dataset=as.data.frame(as.matrix(dtm))
dataset$liked=dataset_original$Liked### adding the dependent variable into datadrame
dataset
##encoding the target feature as factor
dataset$liked=factor(dataset$liked,levels=c(0,1))
##SPLITTING THE DATASET INTO TRAINING AND TESTING
library(caTools)
split=sample.split(dataset$liked,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
##fitting random forest into data
library(randomForest)
classifier=randomForest(x=training_set[-692],y=training_set$liked,ntree=10)
##predicting the test results
y_pred=predict(classifier,newdata=test_set[-692])
##making the confusion matrix
cm=table(test_set[,692],y_pred)
cm
##accuracy
accuracy=(87+77/200)
accuracy

install.packages("wordcloud")
install.packages("tm")
install.packages("ggplot2")
library(wordcloud)
library(tm)
library(ggplot2)
bees=Corpus(DirSource("E:/projects/bee"))
inspect(bees)
bees=tm_map(bees, stripWhitespace)

bees=tm_map(bees, tolower)

bees=tm_map(bees, removeWords, stopwords("english"))

bees=tm_map(bees, stemDocument)
bees=tm_map(bees, removeWords,c(""noble", "lord""))
wordcloud(bees,random.order=FALSE, colors=brewer.pal(6, "Dark2"),min.freq=10, scale=c(4,.2),rot.per=.15,max.words=100)
          
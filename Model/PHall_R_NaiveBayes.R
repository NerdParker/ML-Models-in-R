install.packages("tm")
install.packages("NLP")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("e1071")
install.packages("gmodels")
install.packages("quanteda")
install.packages("pROC")

library(tm) 
library(SnowballC)
library(NLP)
library(wordcloud) 
library(e1071)
library(gmodels)
library(caret)
library(pROC)

#importing data
temp<- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip",temp)
msgfile<- unz(temp, "SMSSpamCollection")
spam<- read.csv2(msgfile, header= FALSE, sep= "\t", quote= "", col.names= c("type","text"), stringsAsFactors= FALSE) #or use read.csv()
unlink(temp)

spam$type <-factor(spam$type)
convert_counts <-function(x) {
  x <-ifelse(x > 0, "YES", "No")
}

#graph of ham/spam distribution
library(ggplot2)
theme_set(theme_bw())
ggplot(aes(x=type),data=spam) +
  geom_bar(fill="pink",width=0.33)

#corpus/source object creation
spam_corpus <- VCorpus(VectorSource(spam$text))
#transform all letters to lowercase
clean_corpus <-tm_map(spam_corpus, content_transformer(tolower))
#remove all numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
#remove all stopwords
clean_corpus <-tm_map(clean_corpus, removeWords, stopwords())
#remove all punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
#remove stem words
clean_corpus <- tm_map(clean_corpus, stemDocument)
#remove whitespace
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

#Document Term Matrix, creates a data structure of rows and columns out of the document
spam_dtm <-DocumentTermMatrix(clean_corpus)
spam_dtm

#test and train data sets and labels
spam_train <- spam_dtm[1:4000, ]
spam_test <- spam_dtm[4001:5574, ]
spam_train_labels <- spam[1:4000, ]$type
spam_test_labels <- spam[4001:5574, ]$type
spam_train_labels
spam_test_labels


#find frequent sms terms
spam_fterms <- findFreqTerms(spam_train, 5)
spam_ftrain <- spam_train[ , spam_fterms]
spam_ftest <- spam_test[ , spam_fterms]
sms_train <- apply(spam_ftrain, MARGIN = 2, convert_counts)
sms_test <- apply(spam_ftest, MARGIN = 2, convert_counts)

#classify with Naive Bayes
spam_classification <- naiveBayes(sms_train, spam_train_labels, laplace = 1)
#predict ham and spam
sms_testpredict <-predict(spam_classification, sms_test)
#cross table
CrossTable(sms_testpredict, spam_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


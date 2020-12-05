sms_raw <- read.csv("C:\\Users\\Sanket\\Downloads\\sms_spam.csv", stringsAsFactors = FALSE)
#looking the data
str(sms_raw)
#
sms_raw$type <- factor(sms_raw$type)
# 
str(sms_raw$type)
#
table(sms_raw$type)
#Package install
install.packages("tm") 
library('tm')
#
sms_corpus <- Corpus(VectorSource(sms_raw$text))
#
print(sms_corpus)
inspect(sms_corpus[1:3])
#
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())

> corpus_clean <- tm_map(corpus_clean, removePunctuation)


corpus_clean <- tm_map(corpus_clean, stripWhitespace)
#
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5574, ]
#
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5574, ]


#
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5574]

#
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
install.packages("wordcloud")
library('wordcloud')
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)


spam <- subset(sms_raw_train, type == "spam")   
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
#


findFreqTerms(sms_dtm_train, 5)
library('dict')
sms_freq_words<-findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}


sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                    convert_counts)

install.packages("e1071")
library("e1071")

sms_classifier <- naiveBayes(sms_train, sms_train_labels)



sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5574, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))



sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
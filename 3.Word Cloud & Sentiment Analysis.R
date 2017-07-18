#Text Minning
#load libraries
library("stringr")
library(tm)
library("wordcloud2")
library("wordcloud")
library(slam)
library(sentimentr)

#Load comments/text
reason = data.frame(reason_for_loan = data$X16)


#Delete the leading spaces
reason$reason_for_loan = str_trim(reason$reason_for_loan)

##Pre-processing
#convert comments into corpus
reasonCorpus = Corpus(VectorSource(reason$reason_for_loan))
writeLines(as.character(reasonCorpus[[1]]))

#case folding
reasonCorpus = tm_map(reasonCorpus, tolower)

#remove stop words
reasonCorpus = tm_map(reasonCorpus, removeWords, stopwords('english'))

#remove punctuation marks
reasonCorpus = tm_map(reasonCorpus, removePunctuation)

#remove numbers
reasonCorpus = tm_map(reasonCorpus, removeNumbers)

#remove unnecesary spaces
reasonCorpus = tm_map(reasonCorpus, stripWhitespace)

#convert into plain text
reasonCorpus = tm_map(reasonCorpus, PlainTextDocument)

#create corpus
reasonCorpus = Corpus(VectorSource(reasonCorpus))

##wordcloud
#Remove the defined stop words
reasonCorpus_WC = reasonCorpus
reasonCorpus_WC = tm_map(reasonCorpus, removeWords, c('i','its','it','us','use','want','added','used','using','will','yes','say','can','take','one',
                                                      stopwords('english')))

#Word cloud
wordcloud(reasonCorpus_WC, max.words = 150, scale=c(5, .2), colors=brewer.pal(8, "Dark2"))


#Build document term matrix
tdm = TermDocumentMatrix(reasonCorpus)

#calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq = as.matrix(words_freq)
words_freq = data.frame(words_freq)
words_freq$words = row.names(words_freq)
words_freq = words_freq[,c(2,1)]
row.names(words_freq) = NULL
names(words_freq) = c("Words", "Frequency")
#wordcloud(words_freq$Words, words_freq$Frequency)

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)

#sentiment Analysis
library(RSentiment)
df = calculate_sentiment(reason$reason_for_loan)


#Merging the sentiments with data
data$Sentiment = df$sentiment

#Text Minning on Negative Comments
negative_sentiment = df[which(df$sentiment == 'Very Negative' | df$sentiment=='Negative'),]
#Wordcloud for negative terms
negCorpus = Corpus(VectorSource(negative_sentiment$text))
negCorpus = tm_map(negCorpus, tolower)
negCorpus = tm_map(negCorpus, removeWords, c('will','can','thanks','better','get','also',
                                             'well','good','now', stopwords('english')))
negCorpus = tm_map(negCorpus, removePunctuation)
negCorpus = tm_map(negCorpus, removeNumbers)
negCorpus = tm_map(negCorpus, stripWhitespace)
negCorpus = tm_map(negCorpus, PlainTextDocument)
negCorpus = Corpus(VectorSource(negCorpus))

wordcloud(negCorpus, max.words = 200, scale=c(3, .1), colors=brewer.pal(6, "Dark2"))


save.image("word")
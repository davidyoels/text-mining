setwd("C:/Users/david/My Files/תואר שני - מנס סייבר/שנה ב/סמסטר ב/כריית טקסט/תרגילים/תרגיל 1")

library(tm)
library(stringi)
library(textstem)

# An helper function to print corpus rows
print.corpus <- function(corpus, max.rows, start=1){
  for(i in start:max.rows){
    print(corpus[[i]][1])
  }
}

# Question 1
tweets <- read.csv('Sentiment180_2000.csv') # read csv file
tweets.df<-data.frame(doc_id=seq(1:nrow(tweets)),text=tweets$Text) # convert text column to data frame 
#(doc_id, text) format required in corpus

# Question 2
corpus <- VCorpus(DataframeSource(tweets.df)) # Creates a corpus with the data frame we created

custom.stopwords <- c(stopwords("english")) #words to remove

# Question 3
# An helper function to clean corpus data
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower)) # lower all texts
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "http\\S*", replacement = ""))) # remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "@\\S*", "", replacement = ""))) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "\\*\\S*", "", replacement = ""))) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace) 
  return(corpus)
}

tweets.df <- data.frame(doc_id=seq(1:nrow(tweets)),text=tweets$Text)
head(tweets.df)

corpus.cleaned <- clean.corpus(corpus)

print.corpus(corpus.cleaned, 1)

# Question 4
# Change 1
# An helper function to clean corpus data
clean.corpus.messed1 <- function(corpus){
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "http\\S*", replacement = ""))) # remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "@\\S*", "", replacement = ""))) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "\\*\\S*", "", replacement = ""))) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

corpus.messed.first <- clean.corpus.messed1(corpus)

print.corpus(corpus.messed.first, 1)

# Change 2
# An helper function to clean corpus data
clean.corpus.messed2 <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "http\\S*", replacement = ""))) # remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "@\\S*", "", replacement = ""))) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "\\*\\S*", "", replacement = ""))) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

corpus.messed.second <- clean.corpus.messed2(corpus)

print.corpus(corpus.messed.second, 1)

# Question 5
tdm <- TermDocumentMatrix(corpus.cleaned)

tdm.matrix <- as.matrix(tdm)

inspect(tdm)

# Question 6
terms_vec.sum <- c(rowSums(tdm.matrix))

head(terms_vec.sum)

tdm.matrix.new<-subset(tdm.matrix, terms_vec.sum[row.names(tdm.matrix)] != 1)

tdm.new<-as.TermDocumentMatrix(tdm.matrix.new,weighting = 0.5)
tdm.new

# Question 7

library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #define the function BigramTokenizer
DTM_BGram <- DocumentTermMatrix(corpus.cleaned, control = list(tokenize = BigramTokenizer)) #BigramTokenizer function is passed as a parameter
inspect(DTM_BGram)


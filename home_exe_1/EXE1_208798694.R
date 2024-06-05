setwd("C:/Users/david/My Files/תואר שני - מנס סייבר/שנה ב/סמסטר ב/כריית טקסט/תרגילים/תרגיל 1")

# load libraries
library(tm)
library(stringi)
library(textstem)

options(stringsAsFactors=FALSE) #Don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')

# An helper function to print corpus rows
print.corpus <- function(corpus, start=1, end=1){
  for(i in start:end){
    print(corpus[[i]][1])
  }
}

# An helper function to remove a given pattern from a text
removePattern <- function(x, pattern) gsub(x, pattern = pattern, replacement = "")

urlPattern <- "http\\S*" # URL pattern
mentionPattern <- "@\\S*" # Mention pattern
starPattern <- "\\*\\S*" # Star pattern

removeURL <- function (x) removePattern(x, urlPattern) #Removes all URLs starts with http
removeMentionPattern <- function (x) removePattern(x, mentionPattern) # Removes all mentions with @
removeStarPattern <- function (x) removePattern(x, starPattern)# Removes all texts start with *

# Question 1
tweets <- read.csv('Sentiment180_2000.csv') #Read Sentiment180_2000 csv file
tweets.df<-data.frame(doc_id=seq(1:nrow(tweets)),text=tweets$Text) #Convert "text" column to data frame 
#To create a corpus, the columns doc_id & text are required.

# Question 2
corpus <- VCorpus(DataframeSource(tweets.df)) #Creates a corpus with the data frame created before

# Question 3
custom.stopwords <- c(stopwords("english")) #Words to remove

# An helper function to clear corpus data
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(removeMentionPattern)) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(removeStarPattern)) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, stripWhitespace)

  return(corpus)
}

corpus.cleaned <- clean.corpus(corpus) #Create a cleaned corpus using clean.corpus helper function

print.corpus(corpus.cleaned, 1, 1) #Print the first row in the corpus

# Question 4
# Change 1
# An helper function to clean corpus data - messed filter functions order
# Swap tolower function and custom.stopwords function
clean.corpus.messed1 <- function(corpus){
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removeURL)) # remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(removeMentionPattern)) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(removeStarPattern)) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

corpus.messed.first <- clean.corpus.messed1(corpus) #Create a cleaned corpus using clean.corpus helper function

print.corpus(corpus.cleaned, 1) # Print the first row of the cleaned corpus
print.corpus(corpus.messed.first, 1) # Print the first row of the first messed corpus

# Change 2
# An helper function to clean corpus data - messed filter functions order
# Change stripWhitespace to be the first function
clean.corpus.messed2 <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower)) # lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # remove all the characters after the http
  corpus <- tm_map(corpus, content_transformer(removeMentionPattern)) # remove all the characters after the @
  corpus <- tm_map(corpus, content_transformer(removeStarPattern)) # remove all the characters after the *
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  
  return(corpus)
}

corpus.messed.second <- clean.corpus.messed2(corpus) # call the filter function

print.corpus(corpus.cleaned, 1) # Print the first row of the cleaned corpus
print.corpus(corpus.messed.second, 1) # Print the first row of the second messed corpus

# Question 5
tdm <- TermDocumentMatrix(corpus.cleaned) # Create A TDM for the cleaned corpus

tdm.matrix <- as.matrix(tdm) #Convert TDM to a matrix

inspect(tdm) #Inspect the TDM data, different parameters (sparsity, No. terms, No. documents etc...)

# Question 6
# Terms that appear in less than two document or more than Inf are discarded
dtm <- DocumentTermMatrix(corpus.cleaned, control=list(bounds = list(global = c(2,Inf)))) 

dtm

# Question 7
library(RWeka) # Loads RWeka library

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #Define the function BigramTokenizer
DTM_BGram <- DocumentTermMatrix(corpus.cleaned, control = list(tokenize = BigramTokenizer)) #BigramTokenizer function is passed as a parameter
inspect(DTM_BGram) #Inspect the DTM BGram data, different parameters (sparsity, No. terms, No. documents etc...)


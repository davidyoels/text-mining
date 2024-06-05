setwd("C:/Users")

#commonality/ comparison cloud
options(stringsAsFactors=FALSE) #don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C') #English(US)

library(tm)
library(ggplot2)
library(ggthemes) #has predefined themes and color palettes for ggplot2 visualizations
library(wordcloud)

####load data
company1 <- read.csv('company1.csv') #Q. 1
company2 <- read.csv('company2.csv') #Q. 1

head(company1$text)
head(company2$text)

length(company1)#24
length(company2)#24

company1.vec <- paste(company1$text, collapse=" ")
company2.vec <- paste(company2$text, collapse=" ")


all <- c(company1.vec, company2.vec) #Q. 2
length(all)

corpus <- VCorpus(VectorSource(all))

tdm <- TermDocumentMatrix(corpus) #create a term document matrix, each column is a document
inspect(tdm)

tdm.m <- as.matrix(tdm)

colnames(tdm.m)<-c("comapny1","company2")

display.brewer.all()
pal <- brewer.pal(9, "GnBu")[-(1:4)]

set.seed(1) 

commonality.cloud(tdm, max.words = 200, random.order=FALSE,colors=pal) #Q. 3

comparison.cloud(tdm.m, max.words = 50, random.order=FALSE,title.size = 1,
                 colors=c('black','darkred'), scale=c(3.5,0.25)) #Q. 3

tdm.m # Number of propabilty of each term in the document #Q. 4

#Q. 5 - no

as.dataframe(tdm.m)

common.words <- subset(tdm.m, tdm.m[,1] > 0 & tdm.m[,2] > 0) #Q. 6

tdm.m
common.words

sum <- rowSums(common.words) #Q. 7
sum

sort(sum, decreasing = T)

commonality.cloud(tdm, max.words = 4, random.order=FALSE,colors=pal) #Q. 8

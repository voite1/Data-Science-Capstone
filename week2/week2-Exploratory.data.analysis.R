### Exploratory data analysis

setwd("C:\\Users\\Aleksey\\Documents\\School\\coursera\\Data Science Capstone\\week2")

## Setting the seed
set.seed(63545)

############################### GET DATA ####################################################

## Getting the data
datafile <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'

## Download file if not exists
if(!file.exists('Coursera-SwiftKey.zip')) {
  download.file(datafile, 'Coursera-SwiftKey.zip')
}

## Unzip downloaded file if not already unzipped
if(!file.exists('final')) {
  unzip('./Coursera-SwiftKey.zip')
}

############################### LOAD DATA ##################################################

## Read blogs file
conn <- file('./final/en_US/en_US.blogs.txt', 'r')
blogs <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
close(conn)

## convert to ascii
blogs <- iconv(blogs, "UTF-8", "ASCII", sub="")

## Read news file
conn <- file('./final/en_US/en_US.news.txt', 'rb')
news <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
close(conn)

## convert to ascii
news <- news(blogs, "UTF-8", "ASCII", sub="")

## Read twitter file
conn <- file('./final/en_US/en_US.twitter.txt', 'r')
twitter <- readLines(conn, encoding="latin1", skipNul=TRUE)
close(conn)

## Convert to ascii
twitter <- iconv(twitter, from = "latin1", to = "ASCII", sub="")

## Cleanup data file names and connections
rm(conn, datafile)

############################### BASIC STATS ###############################################

## Count number of words, lines, and characters in files and display results to the users
system("wc ./final/en_US/en_US.blogs.txt",intern=TRUE)
system("wc ./final/en_US/en_US.news.txt",intern=TRUE)
system("wc ./final/en_US/en_US.twitter.txt",intern=TRUE)

## Display number of lines in files
length(blogs) 
length(news)
length(twitter)

############################### SAMPLING DATA ##############################################

## Reduce the data side by sampling for further analysis
library(caTools)

indx <- sample.split(blogs,SplitRatio= .005, group=NULL)
blogs <- blogs[indx]

indx <- sample.split(news,SplitRatio =.005, group=NULL)
news <- news[indx]

indx <- sample.split(twitter,SplitRatio= .005, group=NULL)
twitter <- twitter[indx]

## cleanup 
rm(indx)

############################### CREATE CORPORA #############################################
library(tm)
library(RWeka)
library(slam)

blogs.c <- VCorpus(VectorSource(blogs))
news.c <- VCorpus(VectorSource(news))
twitter.c <- VCorpus(VectorSource(twitter))

## Merge corpora
data.c <- c(blogs.c, news.c, twitter.c)

## cleanup
rm(twitter, news, blogs, twitter.c, blogs.c, news.c)

############################## CLEANUP DATA ################################################
## using functions from tm library
library(tm)

data.c <- tm_map(data.c, stripWhitespace)
data.c <- tm_map(data.c, content_transformer(tolower))
data.c <- tm_map(data.c, removePunctuation)
data.c <- tm_map(data.c, removeNumbers)      

############################### GENERATE AND TRANSFORM UNIGRAMS FOR DISPLAY #################
library(tm)
library(RWeka)

UnigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
BigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
TrigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }
ForgramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4)) }

## convert docmatrix to a dataframe with word and freq
buildDataFrame <- function(x) {
  item <- as.data.frame(slam::row_sums(x, na.rm=T))
  colnames(item)<- "Frequency"
  item <- cbind(item = rownames(item),item)
  rownames(item) <- NULL
  item
}

tdm <- TermDocumentMatrix(data.c, control=list(tokenize = UnigramTokenizer))
tdm2 <- TermDocumentMatrix(data.c, control=list(tokenize = BigramTokenizer))
tdm3 <- TermDocumentMatrix(data.c, control=list(tokenize = TrigramTokenizer))
tdm4 <- TermDocumentMatrix(data.c, control=list(tokenize = ForgramTokenizer))

words <- buildDataFrame(tdm)
words2 <- buildDataFrame(tdm2)
words3 <- buildDataFrame(tdm3)
words4 <- buildDataFrame(tdm4)

## sort the dataframe by most used words for printing
words <- words[order(-words$Frequency),]
head(words)

words2 <- words2[order(-words2$Frequency),]
head(words2)

words3 <- words3[order(-words3$Frequency),]
head(words3)

words4 <- words4[order(-words4$Frequency),]
head(words4)

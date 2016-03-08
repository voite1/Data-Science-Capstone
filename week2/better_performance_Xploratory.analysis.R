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

## Load data
if(!file.exists('subset.RData')) {
    ## Read blogs file
    conn <- file('./final/en_US/en_US.blogs.txt', 'r')
    blogs <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
    close(conn)
  
    ## Read news file
    conn <- file('./final/en_US/en_US.news.txt', 'rb')
    news <- readLines(conn, skipNul=TRUE)
    close(conn)
  
    ## Read twitter file
    conn <- file('./final/en_US/en_US.twitter.txt', 'r')
    twitter <- readLines(conn, encoding="latin1", skipNul=TRUE)
    close(conn)
  
    ## Convert twitter data to UTF-8 encoding
    twitter <- iconv(twitter, from = "latin1", to = "UTF-8", sub="")
  
    ## remove non UTF-8 characters
    library(stringi)
  
    twitter <- stri_replace_all_regex(twitter, "\u2019|\u201c|\u201d|u201f|``|`",'"')
  
    ## Cleanup data file names and connections
    rm(conn, datafile)
    
    ## Reduce the data side by sampling for further analysis
    library(caTools)
    
    indx <- sample.split(blogs,SplitRatio= .01, group=NULL)
    blogs <- blogs[indx]
    
    indx <- sample.split(news,SplitRatio =.01, group=NULL)
    news <- news[indx]
    
    indx <- sample.split(twitter,SplitRatio= .01, group=NULL)
    twitter <- twitter[indx]
    
    ## cleanup 
    rm(indx)
    
    ## concatenate samples
    data.c <- c(blogs, news, twitter)
    
    ## save data subset
    save(data.c, file = "subset.RData")
}

data.c <- load("subset.RData", verbose = FALSE)

############################### BASIC STATS ###############################################

## Count number of words, characters, and lines in files and display results to the users
system("wc ./final/en_US/en_US.blogs.txt",intern=TRUE)
system("wc ./final/en_US/en_US.news.txt",intern=TRUE)
system("wc ./final/en_US/en_US.twitter.txt",intern=TRUE)

############################### CREATE CORPORA #############################################
library(tm)
library(RWeka)
library(slam)

data.c <- VCorpus(VectorSource(data.c))

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

unigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
BigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
TrigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }
ForgramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))}

## convert docmatrix to a dataframe with word and freq
buildDataFrame <- function(x) {
  item <- as.data.frame(slam::row_sums(x, na.rm=T))
  colnames(item)<- "Frequency"
  item <- cbind(item = rownames(item),item)
  rownames(item) <- NULL
  item
}

tdm <- TermDocumentMatrix(data.c, control=list(tokenize = unigramTokenizer))
tdm2 <- TermDocumentMatrix(data.c, control=list(tokenize = BigramTokenizer))
tdm3 <- TermDocumentMatrix(data.c, control=list(tokenize = TrigramTokenizer))

words <- builtDataFrame(tdm)
words2 <- builtDataFrame(tdm2)
words3 <- buildDataFrame(tdm3)

## sort the dataframe by most used words
library(plyr)
words <- arrange(words,desc(Frequency))
head(words)

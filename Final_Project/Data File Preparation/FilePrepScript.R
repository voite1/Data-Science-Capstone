setwd('C:\\Users\\db345c\\Desktop\\Data Science Capstone\\Modeling')

############## Download and unzip files ##########################

# Download file if not exists
if(!file.exists('Coursera-SwiftKey.zip')) {
    
    # Getting the data
    datafile.url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    
    # Fetch and save the file
    download.file(datafile.url, 'Coursera-SwiftKey.zip')
    
    # Clean unused variables
    rm(datafile.url)
}

## Unzip and sort downloaded files
if(!file.exists('final')) {
    
    # Unzip downloaded file
    unzip('./Coursera-SwiftKey.zip')
    
    # Remove unused directories and files
    unlink("./final/de_DE", recursive=TRUE)
    unlink("./final/ru_RU", recursive=TRUE)
    unlink("./final/fi_FI", recursive=TRUE)
}


########## Read original data files in R #########################

conn <- file('./final/en_US/en_US.blogs.txt', 'r')
blogs <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
close(conn)

# Convert to ascii
blogs <- iconv(blogs, from="UTF-8", to="ASCII")

# Read news file
conn <- file('./final/en_US/en_US.news.txt', 'rb')
news <- readLines(conn, encodin="UTF-8", skipNul=TRUE)
close(conn)

# Convert to ascii
news <- iconv(news, from="UTF-8", to="ASCII")

# Read twitter file
conn <- file('./final/en_US/en_US.twitter.txt', 'r')
twitter <- readLines(conn, encoding="latin1", skipNul=TRUE)
close(conn)

# Convert to ascii
twitter <- iconv(twitter, from = "latin1", to = "ASCII")


######### Create a subset of data for analysis ####################

# Load library to get samples (sed for 3 calls below)
library(caTools, quietly=TRUE)

# get 10% sample of blogs
indx <- sample.split(blogs, SplitRatio= .15)
blogs <- blogs[indx]

# get 10% sample of news
indx <- sample.split(news, SplitRatio = .15)
news <- news[indx]

# get 10% sample of twitter
indx <- sample.split(twitter, SplitRatio= .15)
twitter <- twitter[indx]


############# Save subset of files to work with ###################

# create subset directory
if(file.exists('subset')) {
    unlink('subset', recursive=TRUE)
    dir.create('./subset')
} else {
    dir.create('./subset') 
}

# write data to a file
conn <- file("./subset/blogs.txt")
writeLines(blogs, conn)
close(conn)

conn <- file("./subset/news.txt")
writeLines(news, conn)
close(conn)

conn <- file("./subset/twitter.txt")
writeLines(twitter, conn)
close(conn)


############## Clean R Workspace  ##################################

# Remove unused variables
rm(indx, conn, blogs, news, twitter) 


################## Loading created text document ###################

library(tm, quietly=TRUE)
library(SnowballC, quietly=TRUE)

files <- DirSource("./subset")

corpus <- VCorpus(x=files)

# Clean corpus: function to remove specified patterns
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Clean corpus: function to remove substitute patterns
translate <- content_transformer(function(x, pattern, sub_pattern) gsub(pattern, sub_pattern, x))

# Get build the list of profanity words
conn <- file("./profanity_list.txt")
profanity <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
close(conn)

# remove "" from profanity and add 'NA' to profanity list
# it appears when encoding original files, many "NA" lines werer generated
length(profanity) <- length(profanity) -1
profanity <- c(profanity, "NA")

# Clean corpus removing text that is not needed
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace,"\\|")
corpus <- tm_map(corpus, toSpace, "<")
corpus <- tm_map(corpus, toSpace, ">")
corpus <- tm_map(corpus, content_transformer(tolower))

# Expand English contractions
corpus <- tm_map(corpus, translate, "couldn't", "could not")
corpus <- tm_map(corpus, translate, "didn't", "did not")
corpus <- tm_map(corpus, translate, "doesn't", "does not")
corpus <- tm_map(corpus, translate, "don't", "do not")
corpus <- tm_map(corpus, translate, "hadn't", "had not")
corpus <- tm_map(corpus, translate, "hasn't", "has not")
corpus <- tm_map(corpus, translate, "haven't", "have not")
corpus <- tm_map(corpus, translate, "he's", "he is")
corpus <- tm_map(corpus, translate, "how'd", "how did")
corpus <- tm_map(corpus, translate, "how's", "how is")
corpus <- tm_map(corpus, translate, "i'd", "i would")
corpus <- tm_map(corpus, translate, "i'll", "i will")
corpus <- tm_map(corpus, translate, "i'm", "i am")
corpus <- tm_map(corpus, translate, "i've", "i have")
corpus <- tm_map(corpus, translate, "isn't", "is not")
corpus <- tm_map(corpus, translate, "it'd", "it would")
corpus <- tm_map(corpus, translate, "it'll", "it will")
corpus <- tm_map(corpus, translate, "it's", "it is")
corpus <- tm_map(corpus, translate, "mayn't", "may not")
corpus <- tm_map(corpus, translate, "might've", "might have")
corpus <- tm_map(corpus, translate, "mightn't", "might not")
corpus <- tm_map(corpus, translate, "must've", "must have")
corpus <- tm_map(corpus, translate, "mustn't", "must not")
corpus <- tm_map(corpus, translate, "needn't", "need not")
corpus <- tm_map(corpus, translate, "shan't", "shall not")
corpus <- tm_map(corpus, translate, "she'd", "she would")
corpus <- tm_map(corpus, translate, "she'll", "she will")
corpus <- tm_map(corpus, translate, "she's", "she is")
corpus <- tm_map(corpus, translate, "should've", "should have")
corpus <- tm_map(corpus, translate, "shouldn't", "should not")
corpus <- tm_map(corpus, translate, "so's", "so is")
corpus <- tm_map(corpus, translate, "that's", "that is")
corpus <- tm_map(corpus, translate, "there's", "there is")
corpus <- tm_map(corpus, translate, "they'll", "they will")
corpus <- tm_map(corpus, translate, "they're", "they are")
corpus <- tm_map(corpus, translate, "wasn't", "was not")
corpus <- tm_map(corpus, translate, "we'd", "we would")
corpus <- tm_map(corpus, translate, "we'll", "we will")
corpus <- tm_map(corpus, translate, "we're", "we are")
corpus <- tm_map(corpus, translate, "we've", "we have")
corpus <- tm_map(corpus, translate, "weren't", "were not")
corpus <- tm_map(corpus, translate, "what're", "what are")
corpus <- tm_map(corpus, translate, "what's", "what is")
corpus <- tm_map(corpus, translate, "what've", "what have")
corpus <- tm_map(corpus, translate, "when's", "when is")
corpus <- tm_map(corpus, translate, "where's", "where is")
corpus <- tm_map(corpus, translate, "who'll", "who will")
corpus <- tm_map(corpus, translate, "who's", "who is")
corpus <- tm_map(corpus, translate, "why's", "why is")
corpus <- tm_map(corpus, translate, "won't", "will not")
corpus <- tm_map(corpus, translate, "won't've", "will not have")
corpus <- tm_map(corpus, translate, "would've", "would have")
corpus <- tm_map(corpus, translate, "wouldn't", "would not")
corpus <- tm_map(corpus, translate, "wouldn't've", "would not have")
corpus <- tm_map(corpus, translate, "you'll", "you will")
corpus <- tm_map(corpus, translate, "you're", "you are")

# Remove NA on a single lines
corpus <- tm_map(corpus, translate, "^NA$", " ")

# Remove punctuation, numbers and profanity
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, profanity)
# corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, stripWhitespace)

# Create tokenizers
library(RWeka, quietly=TRUE)
BigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
TrigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }
QuadgramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4)) }

# Create corpus using again, this time using VectorSource
corpus <- Corpus(VectorSource(corpus))

# Create TDM for bi-grams
tdm2 <- TermDocumentMatrix(corpus, control=list(tokenize = BigramTokenizer,
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE))
# Remove sparse terms
tdm2 <- removeSparseTerms(tdm2, 0.10)

# Clean java heap
detach("package:RWeka", unload=TRUE)
library(RWeka, quietly=TRUE)

# Create TDM for tri-grams
tdm3 <- TermDocumentMatrix(corpus, control=list(tokenize = TrigramTokenizer,
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE))
# Remove Sparse ters
tdm3 <- removeSparseTerms(tdm3, 0.10)

# Clean java heap
detach("package:RWeka", unload=TRUE)
library(RWeka, quietly=TRUE)

# Create TDM for tri-grams
tdm4 <- TermDocumentMatrix(corpus, control=list(tokenize = QuadgramTokenizer,
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE))
# Remove Sparse ters
tdm4 <- removeSparseTerms(tdm4, 0.10)

# Clean java heap
detach("package:RWeka", unload=TRUE)


################## Building data frames #################################

library(slam, quietly=TRUE)

## Function to convert TDM to a dataframe with phrases and frequencies
buildDataFrame <- function(x) {
    
    # Calculate the sum
    item <- as.data.frame(slam::row_sums(x, na.rm=T))
    
    # Name the column containing frequencies
    colnames(item)<- "Frequency"
    
    # Create data frame containing phrases/frequencies
    item <- cbind(item = rownames(item),item)
    
    # Remove row names
    rownames(item) <- NULL
    
    # Return data frame
    item
}

bigramDF <- buildDataFrame(tdm2)
trigramDF <- buildDataFrame(tdm3)
quadgramDF <- buildDataFrame(tdm4)


######################## Save generated n-gram data frames ###############

# create subset directory
if(file.exists('data_frames')) {
    unlink('data_frames', recursive=TRUE)
    dir.create('./data_frames') 
} else {
    dir.create('./data_frames') 
}

# write data to a file
write.csv(bigramDF, "./data_frames/bigrams.csv", row.names=FALSE)
write.csv(trigramDF, "./data_frames/trigrams.csv", row.names=FALSE)
write.csv(quadgramDF, "./data_frames/quadgrams.csv", row.names=FALSE)


################## Transform data frames to have word per column ##########

bigrams <- read.csv("./data_frames/bigrams.csv", stringsAsFactors=FALSE)
trigrams <- read.csv("./data_frames/trigrams.csv", stringsAsFactors=FALSE)
quadgrams <- read.csv("./data_frames/quadgrams.csv", stringsAsFactors=FALSE)

library(stringr)

# Update bigrams data frame
w1 <- word(bigrams$item, 1)
prediction <- word(bigrams$item, 2)
bigrams <- data.frame(w1=w1, prediction=prediction, rank=bigrams$Frequency)

# Update trigrams data frame
w1 <- word(trigrams$item, 1)
w2 <- word(trigrams$item, 2)
prediction <- word(trigrams$item, 3)
trigrams <- data.frame(w1=w1, w2=w2, prediction=prediction, rank=trigrams$Frequency)

# Update quadgrams data frame
w1 <- word(quadgrams$item, 1)
w2 <- word(quadgrams$item, 2)
w3 <- word(quadgrams$item, 3)
prediction <- word(quadgrams$item, 4)
quadgrams <- data.frame(w1=w1, w2=w2, w3=w3, prediction=prediction, rank=quadgrams$Frequency)


############### Save completed data frames as csv files (just in case) #####

# create subset directory
if(file.exists('data_frames_ready')) {
    unlink('data_frames_ready', recursive=TRUE)
    dir.create('./data_frames_ready') 
} else {
    dir.create('./data_frames_ready') 
}

# write data to a file
write.csv(bigrams, "./data_frames_ready/bigrams.csv", row.names=FALSE)
write.csv(trigrams, "./data_frames_ready/trigrams.csv", row.names=FALSE)
write.csv(quadgrams, "./data_frames_ready/quadgrams.csv", row.names=FALSE)


####### Cleanup workspace - removed unused variables ########################

rm(bigramDF, trigramDF, quadgramDF, conn, corpus, files)
rm(prediction, profanity, tdm2, tdm3, tdm4, w1, w2, w3)
rm(BigramTokenizer, TrigramTokenizer, QuadgramTokenizer)
rm(toSpace, translate, buildDataFrame)


#### AT THIS POINT THE FILES FOR PREDITION ARE READY ########################

# Saving RData file for faster loading
save(list = ls(all.names = TRUE), file = "./data_frames_ready/dataframes.RData", envir = .GlobalEnv)

############ Ready to implement prediction algorythm ########################

# Testing loading saved file to .GlobalEnv
rm(list=ls())
load("./data_frames_ready/dataframes.RData", envir = .GlobalEnv, verbose = FALSE)

# The line above also works <--- USE THIS ONE TO LOAD IN CURRENT ENVIRONMENT (I THINK)
rm(list=ls())
load("./data_frames_ready/dataframes.RData", envir = parent.frame(), verbose = FALSE)


############# END OF FILE ##################################################

### Spam detection (playing around)

setwd("C:\\Users\\Aleksey\\Documents\\School\\coursera\\Data Science Capstone\\test")

library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(caret)
library(RTextTools)
library(topicmodels)
library(slam)
library(RKEA)

##-----Load Data Sets----- PAGE 8 of the slides
# Load email list
texts = read.csv("text_messages.csv", stringsAsFactors = FALSE)

# Change to lower case:
texts$Message = tolower(texts$Message)

# Remove punctuation
texts$Message = sapply(texts$Message, function(x) gsub("'", "", x))
# Now the rest of the punctuation
texts$Message = sapply(texts$Message, function(x) gsub("[[:punct:]]", " ", x))

# Remove numbers
texts$Message = sapply(texts$Message, function(x) gsub("\\d","",x))

# Remove extra white space, so we can split words by spaces
texts$Message = sapply(texts$Message, function(x) gsub("[ ]+"," ",x))

# Remove non-ascii
texts$Message = iconv(texts$Message, from="latin1", to="ASCII", sub="")

# remove stopwords
stopwords()
my_stops = as.character(sapply(stopwords(), function(x) gsub("'","",x)))
texts$Message = sapply(texts$Message, function(x){
  paste(setdiff(strsplit(x," ")[[1]],stopwords()),collapse=" ")
})

# Remove extra white space again:
texts$Message = sapply(texts$Message, function(x) gsub("[ ]+"," ",x))

# Stem words:
texts$message_stem = sapply(texts$Message, function(x){
  paste(setdiff(wordStem(strsplit(x," ")[[1]]),""),collapse=" ")
})


# Create a Corpus (matrix of frequent terms)
##-----Text Corpus----- PAGE 10 in presentation
# We have to tell R that our collection of reviews is really a corpus.
text_corpus = Corpus(VectorSource(texts$message_stem))

# Build a Document Term Matrix
# Terms        Docs
#            ... 32 33 34 35 36 37 38 39 ...
# about       0  0  1  1  1  0  1  2  1  0
# wut         0  0  0  0  0  0  1  0  0  0
# lol         0  0  0  1  0  0  0  0  0  0
# u           0  0  0  0  0  0  0  0  0  0
# rofl        0  0  1  0  0  0  0  0  0  0
# ...

text_term_matrix = DocumentTermMatrix(text_corpus)

dim(text_term_matrix)

# Save Matrix (This is mostly empty)
text_corpus_mat = as.matrix(text_term_matrix)
dim(text_corpus_mat)

# Convert to Data Frame
text_frame = as.data.frame(text_corpus_mat)

# Use only the most common terms
which_cols_to_use = which(colSums(text_corpus_mat) > 10)

text_frame = text_frame[,which_cols_to_use]

# Convert to factors: (to use with naive base)
text_frame = as.data.frame(lapply(text_frame, as.factor))

# Add the response
text_frame$type = texts$Type

# Split into train/test set
train_ind = sample(1:nrow(text_frame), round(0.8*nrow(text_frame)))
train_set = text_frame[train_ind,]
test_set = text_frame[-train_ind,]

# Compute Naive Bayes Model
text_nb = naiveBayes(as.factor(type) ~ ., data = train_set)
test_predictions = predict(text_nb, newdata = test_set, type="class")

# Look at outcomes:
confusionMatrix(test_predictions, as.factor(test_set$type))

# Do a prediction
important_words = setdiff(names(text_frame), "type")

sample_text = "Please call asap for free consultation!"
sample_text = tolower(sample_text)
sample_text = gsub("'", "", sample_text)
sample_text = gsub("[[:punct:]]", " ", sample_text)
sample_text = gsub("\\d","",sample_text)
sample_text = gsub("[ ]+"," ",sample_text)
sample_text = iconv(sample_text, from="latin1", to="ASCII", sub="")
sample_text = gsub("[ ]+"," ",sample_text)
sample_text = paste(setdiff(wordStem(strsplit(sample_text," ")[[1]]),""),collapse=" ")

# Create occurence vector of important words
sample_occurences = sapply(important_words, function(x){
  return(as.numeric(x%in%strsplit(sample_text," ")[[1]]))
})

sample_data = as.data.frame(t(sample_occurences))
sample_prediction = predict(text_nb, newdata = sample_data, type = "class")

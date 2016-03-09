## setwd("C:\\Users\\Aleksey\\Documents\\School\\coursera\\Data Science Capstone\\week2")
setwd("C:\\Users\\db345c\\Desktop\\Data Science Capstone\\week2")

##-----Load Libraries-----
library(tm)
library(SnowballC)


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

my_data <- tolower(my_data)

my_data <- gsub("'", "", my_data)

my_data <- gsub("\\d", "", my_data)

my_data <- gsub("^na$", "", my_data)

my_data <- my_data[my_data != ""] 

my_data <- gsub("[[:punct:]]", " ", my_data)

my_data <- gsub("[ ]+"," ", my_data)







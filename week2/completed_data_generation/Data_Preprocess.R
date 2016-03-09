setwd("C:\\Users\\Aleksey\\Documents\\School\\coursera\\Data Science Capstone\\week2\\completed_data_generation")
##setwd("C:\\Users\\db345c\\Desktop\\Data Science Capstone\\week2")

set.seed(63545)

## The purpose of this function is to download, extract, and save
## the only data needed
getDataFiles <- function() {
    
    # Getting the data
    datafile.url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    
    # Download file if not exists
    if(!file.exists('Coursera-SwiftKey.zip')) {
        
        # Fetch and save the file
        download.file(datafile, 'Coursera-SwiftKey.zip')
    }
    
    ## Unzip and sort downloaded files
    if(!file.exists('final')) {
        
        # Unzip downloaded file
        unzip('./Coursera-SwiftKey.zip')
        
        # Remove unused directories and files
        unlink("./final/de_DE", recursive=TRUE)
        unlink("./final/ru_RU", recursive=TRUE)
        unlink("./final/fi_FI", recursive=TRUE)
        
        # Clean unused variables
        rm(datafile.url)
    }
}

## The purpose of tis function is to create a subset of data needed
## for processing
createDataSubset <- function() {
    
    # Check if subset of data is created in 'subset', if so, skipp processing
    # and load subset file directly
    if(file.exists('subset')) {
        
        # load subset of data
        conn <- file("./subset/data_subset.txt", 'rb')
        txt_data <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
        close(conn)
        
        # convert to ascii
        txt_data <- iconv(txt_data, from = "latin1", to = "ASCII")
        
        # remove unused variables
        rm(conn)
        
    } else {
        
        # Process original files
        
        # Read blogs file
        conn <- file('./final/en_US/en_US.blogs.txt', 'r')
        blogs <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
        close(conn)
        
        # convert to ascii
        blogs <- iconv(blogs, from="UTF-8", to="ASCII")
        
        # Read news file
        conn <- file('./final/en_US/en_US.news.txt', 'rb')
        news <- readLines(conn, encodin="UTF-8", skipNul=TRUE)
        close(conn)
        
        # convert to ascii
        news <- iconv(news, from="UTF-8", to="ASCII")
        
        # Read twitter file
        conn <- file('./final/en_US/en_US.twitter.txt', 'r')
        twitter <- readLines(conn, encoding="latin1", skipNul=TRUE)
        close(conn)
        
        # Convert to ascii
        twitter <- iconv(twitter, from = "latin1", to = "ASCII")
        
        # Load library to get samples (sed for 3 calls below)
        library(caTools, quietly=TRUE)
        
        # get 10% sample of blogs
        indx <- sample.split(blogs, SplitRatio= .15, group=NULL)
        blogs <- blogs[indx]
        
        # get 10% sample of news
        indx <- sample.split(news, SplitRatio = .15, group=NULL)
        news <- news[indx]
        
        # get 10% sample of twitter
        indx <- sample.split(twitter, SplitRatio= .15, group=NULL)
        twitter <- twitter[indx]
        
        # combine three subsets into one
        txt_data <- c(blogs, news, twitter)
        
        # create subset directory
        dir.create('subset')
        
        # write data to a file
        conn<-file("./subset/data_subset.txt")
        writeLines(txt_data, conn)
        close(conn)
        
        # Remove unused variables
        rm(indx, conn, blogs, news, twitter) 
    }
    
    # return data
    txt_data
}

## The purpose of this function is to clean the text
cleanDataSubset <- function(data_subset) {
    
    # Load required libraries
    library(tm, quietly=TRUE)
    library(SnowballC, quietly=TRUE)
    
    # Convert to lower case
    data_subset <- tolower(data_subset)
    
    # Remove ' character
    data_subset <- gsub("'", "", data_subset)
    
    # Remove digits
    data_subset <- gsub("\\d", "", data_subset)
    
    # Remove text in lines only containing 'na'
    data_subset <- gsub("^na$", "", data_subset)
    
    # Remove line containing 'na' only
    data_subset <- my_data[data_subset != ""] 
    
    # Remove punctuation
    data_subset <- gsub("[[:punct:]]", " ", data_subset)
    
    # Remove extra spaces
    data_subset <- gsub("[ ]+"," ", data_subset)
}



### Functions shold be used in the following sequence as shown below

getDataFiles()

my_data <- createDataSubset()

my_data <- cleanDataSubset(my_data)

######################################################################################
################# At this point corpus is ready to be created ########################
############ starting from this point the script needs to be run manually ############
############ to generate data files for bi-, tri-, and four- grams ###################
########### the RWeka library tends to break if run as part of the scrip #############
########################## running manually, line by line - works ####################
######################################################################################

# Load required libraries
library(RWeka, quietly=TRUE)

# Create corpus
crp <- VCorpus(VectorSource(my_data))

# Create n-grams (2, 3, and 4)
BigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
TrigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }
ForgramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4)) }

# Commands below run for several hours on i7 core, 64bit, 16GB RAM
tdm2 <- TermDocumentMatrix(crp, control=list(tokenize = BigramTokenizer))
tdm3 <- TermDocumentMatrix(crp, control=list(tokenize = TrigramTokenizer))
tdm4 <- TermDocumentMatrix(crp, control=list(tokenize = ForgramTokenizer))

# Load required libraris
library(slam, quietly=TRUE)

## convert docmatrix to a dataframe with word and freq
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

# Generate data frames from TermDocumentMetrix documents
bi_gram_df <- buildDataFrame(tdm2)
tri_gram_df <- buildDataFrame(tdm3)
four_gram_df <- buildDataFrame(tdm4)

# Sort data frames in descending order by phrase frequency
# and view first 10 records.
bi_gram_df <- bi_gram_df[order(-bi_gram_df$Frequency),]
head(bi_gram_df)

tri_gram_df <- tri_gram_df[order(-tri_gram_df$Frequency),]
head(tri_gram_df)

four_gram_df <- four_gram_df[order(-four_gram_df$Frequency),]
head(four_gram_df)

# Create directory to store data from sorted data frames
dir.create('data_frames_ready')

# Write data frames to corresponding text files (the files are 
# sorted by the word frequencies in descending order). Thise files need
# to be reduced by saving only 20,000 first rows from each file.
write.csv(bi_gram_df, "./data_frames_ready/bi_gram_df.csv", row.names=FALSE)
write.csv(tri_gram_df, "./data_frames_ready/tri_gram_df.csv", row.names=FALSE)
write.csv(four_gram_df, "./data_frames_ready/four_gram_df.csv", row.names=FALSE)

# Taking 20,000 rows from each file and saving the files in the same
# directory as above.  The files generated here will be used for 
# the final shiny app.
bi_gram_df <- read.csv("./data_frames_ready/bi_gram_df.csv", header = TRUE)
tri_gram_df <- read.csv("./data_frames_ready/tri_gram_df.csv", header = TRUE)
four_gram_df <- read.csv("./data_frames_ready/four_gram_df.csv", header = TRUE)

bi_gram_df_r <- bi_gram_df[1:100000,]
tri_gram_df_r <- tri_gram_df[1:100000,]
four_gram_df_r <- four_gram_df[1:100000,]

write.csv(bi_gram_df_r, "./data_frames_ready/bi_gram_df_r.csv", row.names=FALSE)
write.csv(tri_gram_df_r, "./data_frames_ready/tri_gram_df_r.csv", row.names=FALSE)
write.csv(four_gram_df_r, "./data_frames_ready/four_gram_df_r.csv", row.names=FALSE)


### Attempting to read data from manually zipped data files (see above)
### Experimenting with compressing data
# bi_gram_df <- read.table(unz("data_frames_ready1/bi_gram_df.zip", "bi_gram_df.csv"), header=T, sep=",")
# tri_gram_df <- read.table(unz("data_frames_ready1/tri_gram_df.zip", "tri_gram_df.csv"), header=T, sep=",")
# four_gram_df <- read.table(unz("data_frames_ready1/four_gram_df.zip", "four_gram_df.csv"), header=T, sep=",")

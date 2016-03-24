#setwd('C:\\Users\\db345c\\Desktop\\Data Science Capstone\\Algorythm_Modeling')
setwd('C:/Users/Aleksey/Documents/School/coursera/Data Science Capstone/Final_Project/3. Shiny UI/Attempt2-MLE')

load("./data/dataframes.RData")

# get the prediction for the phrase entered
getPrediction <- function(phrase) {
    
    # empty data frame to hold the search results
    df <- data.frame()
    
    # convert to lower case
    phrase <- tolower(phrase)
    
    # split string
    input <- strsplit(phrase, " ")[[1]]
    
    # remove extra white spaces (if any)
    input < gsub("^\\s+|\\s+$", "", input)
    
    # if input phrase is exactly one word
    if(length(input) == 1) {
        
        # search bigrams
        df <- getPrediction1(input[1])
    }
    
    # if input phrase is exactly two words
    if(length(input) == 2) {
        
        # search trigrams
        df <- getPrediction2(input[1], input[2])
    }

    # if input phrase is exactly three words
    if(length(input) == 3) {
        
        # search quadgrams
        df <- getPrediction3(input[1], input[2], input[3])
    }
    
    # if input phrase is longer thatn three words
    if(length(input) > 3) {
        
        # extract last three words
        tmp <- tail(input, 3)
        
        # search quadgrams
        df <- getPrediction3(tmp[1], tmp[2], tmp[3])

    }
    
    # if not results found, error message for prediction
    if (dim(df)[1] < 1) {
        df <- data.frame(w1="", prediction="NOT FOUND")
        df$prediction <- as.character(df$prediction)
    }
    
    return(df[1,]$prediction)
    
}

# Get prediction for single word
getPrediction1 <- function(in1) {
    df1 <- subset(bigrams, w1 == in1)
    
    # if there are hits
    if(length(df1$w1) > 0) {
      # Calculate MLE
      # rename rank field to numerator
      names(df1)[names(df1)=="rank"] <- "numerator"
      
      # merge data frames on the second word of the bigram and a signle word of unigram
      df1 <- merge(df1, unigrams, by.x="w2", by.y="w1")
      
      # rename rank from bigrams to denominator
      names(df1)[names(df1)=="rank"] <- "denominator"
      names(df1)[names(df1)=="w2"] <- "prediction"
      
      # calculate rank
      df1$rank <- df1$numerator / df1$denominator
      
      # sort by rank descending and take only first 6 rows
      df1 <- head(df1[order(-df1$rank),])
    }
    
    return(df1)
}

# Get prediction for two words
getPrediction2 <- function(in1, in2) {
    df2 <- subset(trigrams, w1 == in1 & w2 == in2)
    
    if(length(df2$w1) < 1) {
        df2 <- getPrediction1(in2)
    } else {
      # calculate MLE
      names(df2)[names(df2)=="rank"] <- "numerator"
      
      # merge data frames
      df2 <- merge(df2, unigrams, by.x="w3", by.y="w1")
      
      # rename rank to denominator
      names(df2)[names(df2)=="rank"] <- "denominator"
      names(df2)[names(df2)=="w3"] <- "prediction"
      
      # calculate rank
      df2$rank <- df2$numerator / df2$denominator
      
      # sort by rank descending and take only first 6 rows
      df2 <- head(df2[order(-df2$rank),])
      
    }
    return(df2)
}

# Get prediction for three words
getPrediction3 <- function(in1, in2, in3) {
    df3 <- subset(quadgrams, w1 == in1 & w2 == in2 & w3 == in3)
    
    if(length(df3$w1) < 1) {
        df3 <- getPrediction2(in2, in3)
    } else {
      # calculate MLE
      names(df3)[names(df3)=="rank"] <- "numerator"
      
      # Merge data frames
      df3 <- merge(df3, unigrams, by.x="w4", by.y="w1")
      
      # rename rank from bigrams to denominator
      names(df3)[names(df3)=="rank"] <- "denominator"
      names(df3)[names(df3)=="w4"] <- "prediction"
      
      # calculate rank
      df3$rank <- df3$numerator / df3$denominator
      
      # sort by rank descending and take only first 6 rows
      df3 <- head(df3[order(-df3$rank),])
    }
    return(df3)
}


##### TEST #####
result <- getPrediction("to lose i")
result


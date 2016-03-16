# setwd('C:\\Users\\db345c\\Desktop\\Data Science Capstone\\Algorythm_Modeling')
setwd('C:/Users/Aleksey/Documents/School/coursera/Data Science Capstone/Final_Project/2. Algorithm and Modeling')

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
        
        #print(tmp)
        
        # search quadgrams
        df <- getPrediction3(tmp[1], tmp[2], tmp[3])

    }
    
    # if not results found, error message for prediction
    if (dim(df)[1] < 1) {
        df <- data.frame(w1="", prediction="NOT FOUND")
        df$prediction <- as.character(df$prediction)
    }
    
    print(df)
    
    return(df[1,]$prediction)
    
}

# Get prediction for single word
getPrediction1 <- function(in1) {
    df1 <- subset(bigrams, w1 == in1)
    df1 <- head(df1[order(-df1$rank),])
    return(df1)
}

# Get prediction for two words
getPrediction2 <- function(in1, in2) {
    df2 <- subset(trigrams, w1 == in1 & w2 == in2)
    df2 <- head(df2[order(-df2$rank),])
    if(length(df2$w1) < 1) {
        df2 <- getPrediction1(in2)
    }
    return(df2)
}

# Get prediction for three words
getPrediction3 <- function(in1, in2, in3) {
    df3 <- subset(quadgrams, w1 == in1 & w2 == in2 & w3 == in3)
    df3 <- head(df3[order(-df3$rank),])
    if(length(df3$w1) < 1) {
        df3 <- getPrediction2(in2, in3)
    }
    return(df3)
}


##### TEST #####
result <- getPrediction("soda")
result

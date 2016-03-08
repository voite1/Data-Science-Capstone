setwd("C:\\Users\\Aleksey\\Documents\\School\\coursera\\Data Science Capstone\\week1")

blogs <- suppressWarnings(readLines("../data/en_US.blogs.txt"))
news <- suppressWarnings(readLines("../data/en_US.news.txt"))
twitter <- suppressWarnings(readLines("../data/en_US.twitter.txt"))
length(twitter)

## Quiz 1 question 1
## The en_US.blogs.txt file is how many megabytes?
## file.info("../data/en_US.blogs.txt")$size / 1024 /1024
## [1] 200.4242

## Quiz 1 question 2
## The en_US.twitter.txt has how many lines of text?
##
## length(twitter)

## Quiz 1 question 3
## What is the length of the longest line seen in any of the three en_US data sets? 
## 
## max(nchar(blogs))
## max(nchar(news))
## max(nchar(twitter))

## Quiz 1 question 4
## In the en_US twitter data set, if you divide the number of 
## lines where the word "love" (all lowercase) occurs by the number 
## of lines the word "hate" (all lowercase) occurs, about what do you get?
##
## length(grep(pattern = "love", x = twitter))
## [1] 90956
## length(grep(pattern = "hate", x = twitter))
## [1] 22138
## 90956 / 22138
## [1] 4.108592

## Quiz 1 question 5
## The one tweet in the en_US twitter data set that matches the word "biostats" says what?
##
## biostats <- grep("biostats", twitter)
## twitter[biostats]

## Quiz 1 question 6
##
## 
##sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
## [1] 3
## sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", blogs))
## [1] 0
## sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", news))
## [1] 0
> 

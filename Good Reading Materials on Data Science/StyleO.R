## ----module, echo=FALSE, results="asis"----------------------------------
Module <- sub(".Rnw", "", current_input())
cat(paste0("\\newcommand{\\Module}{", Module, "}"))

## ----echo=FALSE, message=FALSE-------------------------------------------
library(caret) # For Max Kuhn citation.
library(rattle)
library(party)
library(Cubist)

## ----eval=FALSE----------------------------------------------------------
## power_analysis_160720.R

## ----eval=FALSE----------------------------------------------------------
## power_analysis_160720.r

## ----eval=FALSE----------------------------------------------------------
## myFancyPlot.R

## ----eval=FALSE----------------------------------------------------------
## MyFancyPlot.R
## my_fancy_plot.R
## my.fancy.plot.R
## my_fancy_plot.r

## ----eval=FALSE----------------------------------------------------------
## weather.RData

## ----eval=FALSE----------------------------------------------------------
## weather.rdata
## weather.Rdata
## weather.rData

## ----eval=FALSE----------------------------------------------------------
## weather.csv

## ----eval=FALSE----------------------------------------------------------
## weather.CSV

## ----eval=FALSE----------------------------------------------------------
## displayPlotAgain()

## ----eval=FALSE----------------------------------------------------------
## DisplayPlotAgain()
## displayplotagain()
## display.plot.again()
## display_plot_again()

## ----eval=FALSE----------------------------------------------------------
## num.frames <- 10
## num.libs   <- 4

## ----eval=FALSE----------------------------------------------------------
## num_frames <- 10
## numframes <- 10
## numFrames <- 10

## ----eval=FALSE----------------------------------------------------------
## MAX.LINES

## ----eval=FALSE----------------------------------------------------------
## const.max.lines

## ----eval=FALSE----------------------------------------------------------
## min_temp
## wind_gust_speed

## ----eval=FALSE----------------------------------------------------------
## max.pressure
## wind.dir
## WindSpeed

## ----eval=FALSE----------------------------------------------------------
## read.csv(file="data.csv", sep=";", na.strings=".")

## ----eval=FALSE----------------------------------------------------------
## read.csv(file = "data.csv", sep =
##          ";", na.strings
##          = ".")

## ------------------------------------------------------------------------
window_delete <- function(action, window)
{
  if (action %in% c("quit", "ask"))
  {
    ans <- TRUE
    msg <-"Terminate?"
    if (!dialog(msg))
      ans <- TRUE
    else
      if (action == "quit")
        quit(save="no")
      else
        ans <- FALSE
  }
  return(ans)
}

## ------------------------------------------------------------------------
window_delete <- function(action, window)
{
        if (action %in% c("quit", "ask"))
        {
                ans <- TRUE
                msg <- "Terminate?"
                if (!dialog(msg))
                        ans <- TRUE
                else
                        if (action == "quit")
                                quit(save="no")
                        else
                                ans <- FALSE
        }
        return(ans)
}

## ------------------------------------------------------------------------
window_delete <- function(action, window)
{
  if (action %in% c("quit", "ask"))
  {
    ans <- TRUE
    msg <-"Terminate?"
    if (!dialog(msg))
      ans <- TRUE
    else
      if (action == "quit")
        quit(save="no")
      else
        ans <- FALSE
  }
  return(ans)
}

## ------------------------------------------------------------------------
window_delete <- function(action, window)
{
  if (action %in% c("quit", "ask"))
  {
    ans <- TRUE
    msg <-"Terminate?"
    if (!dialog(msg))
      return(TRUE)
    else
      if (action == "quit")
        quit(save="no")
      else
        return(FALSE)
  }
}

## ----eval=FALSE----------------------------------------------------------
## while (blueSky())
## {
##   openTheWindows()
##   doSomeResearch()
## }
## retireForTheDay()

## ----eval=FALSE----------------------------------------------------------
## while (blueSky()) {
##   openTheWindows()
##   doSomeResearch()
## }
## retireForTheDay()

## ----eval=FALSE----------------------------------------------------------
## if (TRUE)
## {
##   42
## }
## else
## {
##   666
## }

## ----eval=FALSE----------------------------------------------------------
## Error: unexpected 'else' in "else"
## 
## ALSO
## 
## > source("tmp.R")
## Error in source("tmp.R") : tmp.R:5:1: unexpected 'else'
## 4: }
## 5: else
##    ^

## ----eval=FALSE----------------------------------------------------------
## myFun <- function()
## {
##   if (TRUE)
##   {
##     42
##   }
##   else
##   {
##     666
##   }
## }
## myFun()

## ----eval=FALSE----------------------------------------------------------
## if (TRUE)
## {
##   42
## } else
## {
##   666
## }

## ----eval=FALSE----------------------------------------------------------
## a       <- 42
## another <- 666
## b       <- mean(x)
## brother <- sum(x)/length(x)

## ----eval=FALSE----------------------------------------------------------
## a <- 42
## another <- 666
## b <- mean(x)
## brother <- sum(x)/length(x)

## ----eval=FALSE----------------------------------------------------------
## library(rattle)
## ds        <- weatherAUS
## names(ds) <- normVarNames(names(ds))
## ds                                         %>%
##   group_by(location)                       %>%
##   mutate(rainfall=cumsum(risk_mm))         %>%
##   ggplot(aes(date, rainfall))               +
##   geom_line()                               +
##   facet_wrap(~location)                     +
##   theme(axis.text.x=element_text(angle=90))

## ----eval=FALSE----------------------------------------------------------
## ds <- weatherAUS
## names(ds) <- normVarNames(names(ds))
## ds %>%
##   group_by(location) %>%
##   mutate(rainfall=cumsum(risk_mm)) %>%
##   ggplot(aes(date, rainfall)) +
##   geom_line() +
##   facet_wrap(~location) +
##   theme(axis.text.x=element_text(angle=90))

## ----eval=FALSE----------------------------------------------------------
## dialPlot <- function(label="UseR!"
##                    , value=78
##                    , dial.radius=1
##                    , value.cex=3
##                    , value.color="black"
##                    , label.cex=3
##                    , label.color="black"
##                     )
## {
##   ...
## }

## ----eval=FALSE----------------------------------------------------------
## dialPlot <- function(label="UseR!", value=78, dial.radius=1,
##                      value.cex=3, value.color="black",
##                      label.cex=3, label.color="black")
## {
##   ...
## }
## 
## dialPlot <- function(label="UseR!",
##                      value=78,
##                      dial.radius=1,
##                      value.cex=3,
##                      value.color="black",
##                      label.cex=3,
##                      label.color="black")
## {
##   ...
## }

## ----eval=FALSE----------------------------------------------------------
## dialPlot(label="UseR!"
##        , value=78
##        , dial.radius=1
##        , value.cex=3
##        , value.color="black"
##        , label.cex=3
##        , label.color="black"
##         )

## ----eval=FALSE----------------------------------------------------------
## dialPlot(label="UseR!", value=78, dial.radius=1,
##          value.cex=3, value.color="black", label.cex=3,
##          label.color="black")
## 
## dialPlot(label="UseR!",
##          value=78,
##          dial.radius=1,
##          value.cex=3,
##          value.color="black",
##          label.cex=3,
##          label.color="black")

## ----eval=FALSE----------------------------------------------------------
## library(dplyr)     # Data wranlging, mutate().
## library(lubridate) # Dates and time, ymd_hm().
## library(ggplot2)   # Visualize data.
## 
## ds <- get(dsname)                                         %>%
##   mutate(timestamp=ymd_hm(paste(date, time)))             %>%
##   ggplot(aes(timestamp, measure))                          +
##   geom_line()                                              +
##   geom_smooth()

## ----eval=FALSE----------------------------------------------------------
## ds <- get(dsname)                                         %>%
##   dplyr::mutate(timestamp=
##                   lubridate::ymd_hm(paste(date, time)))   %>%
##   ggplot2::ggplot(ggplot2::aes(timestamp, measure))        +
##   ggplot2::geom_line()                                     +
##   ggplot2::geom_smooth()


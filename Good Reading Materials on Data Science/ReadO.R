## ----load_packages, message=FALSE----------------------------------------
library(xlsx)    # Read Excel spreadsheets.
library(RCurl)
library(foreign)
library(openxlsx)
library(readxl)  # The definitive Excel reader.
library(readr)   # Efficient reading of data.

## ----checkwd-------------------------------------------------------------
getwd()

## ----list_csv_files------------------------------------------------------
dir(path="data", pattern="*.csv")

## ----read_csv_heart------------------------------------------------------
heart <- read.csv(file=file.path("data", "heart.csv"))

## ----check_csv_heart-----------------------------------------------------
dim(x=heart)
head(x=heart)
tail(x=heart)
str(object=heart)

## ----read_csv_attempt1---------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"))

## ------------------------------------------------------------------------
str(read.csv)

## ----check_csv_attemp1---------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)
summary(stroke)

## ----read_csv_attempt2---------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), sep=";")
dim(stroke)
head(stroke)
str(stroke)

## ------------------------------------------------------------------------
str(read.csv)

## ------------------------------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), sep=";")
stroke <- read.csv(file.path("data", "stroke.csv"), header=TRUE, sep=";")
stroke <- read.csv(file.path("data", "stroke.csv"), TRUE, ";")

## ------------------------------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), ";")

## ----read_csv2-----------------------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"))

## ----review_data_stroke_2------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)

## ----eval=FALSE----------------------------------------------------------
## ?read.csv

## ------------------------------------------------------------------------
read.csv

## ----eval=FALSE----------------------------------------------------------
## ds <- read.csv("stroke.csv", stringsAsFactors=FALSE)

## ----eval=FALSE----------------------------------------------------------
## View(stroke)

## ----eval=FALSE----------------------------------------------------------
## library(RGtk2Extras)
## dfedit(stroke)

## ----eval=FALSE----------------------------------------------------------
## library(Deducer)
## date.viewer()

## ------------------------------------------------------------------------
tail(stroke)

## ----read_csv_stroke_nastrings-------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=".")

## ------------------------------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)

## ------------------------------------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=c(".", "?", " "))

## ------------------------------------------------------------------------
sapply(stroke, class)

## ------------------------------------------------------------------------
classes <- c("factor", "character", "character", "integer", "factor", 
             "factor", "factor", "factor", "factor")
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=".", 
                    colClasses=classes)
sapply(stroke, class)

## ----eval=FALSE----------------------------------------------------------
## fnames <- dir(pattern="\\.csv$")
## dsl    <- lapply(fnames, read.csv)
## ds     <- do.call("rbind", dsl)

## ----eval=FALSE----------------------------------------------------------
## ds <- read.table("clipboard")

## ----eval=FALSE----------------------------------------------------------
## write(weather, file=file.path("data", "myweather.csv"), row.names=FALSE)

## ------------------------------------------------------------------------
save(stroke, file=file.path("data", "stroke.RData"))

## ----eval=FALSE----------------------------------------------------------
## read.fwf()

## ----read_table_url------------------------------------------------------
addr <- file.path("http://www.ats.ucla.edu/stat/r/examples/alda/data",
                  "tolerance1_pp.txt")
tolerance <- read.csv(addr)

## ------------------------------------------------------------------------
dim(tolerance)
head(tolerance)
tail(tolerance)
str(tolerance)
summary(tolerance)

## ----save_tolerance, echo=3----------------------------------------------
# Save tolerance backup in case it goes missing
save(tolerance, file=file.path("data", sprintf(".tolerance_%s.RData", format(Sys.time(), "%y%m%d"))))
save(tolerance, file=file.path("data", "tolerance.RData"))

## ----query_key-----------------------------------------------------------
key <- "0Aonsf4v9iDjGdHRaWWRFbXdQN1ZvbGx0LWVCeVd0T1E"

## ------------------------------------------------------------------------
tt <- getForm("https://spreadsheets.google.com/spreadsheet/pub", 
              hl="en_US", key=key, output="csv")
tt

## ------------------------------------------------------------------------
ds <- read.csv(textConnection(tt))
dim(ds)
head(ds)

## ------------------------------------------------------------------------
library(RCurl)
key   <- "0AmbQbL4Lrd61dER5Qnl3bHo4MkVNRlZ1OVdicnZnTHc"
query <- curlEscape("select *")
addr  <- paste0("http://spreadsheets.google.com/tq?",
                "key=", key, 
                "&tq=", query, 
                "&tqx=out:csv")
addr

## ------------------------------------------------------------------------
ds <- read.csv(addr)


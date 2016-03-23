
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "BigDataO"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))




## ----load_packages, message=FALSE----------------------------------------
library(data.table)   # Efficient data storage
library(dplyr)        # Efficient data manipulation
library(rattle)       # For normVarNames.
#library(rbenchmark)
#library(sqldf)


## ----child-demo, child='documentation.Rnw', eval=TRUE--------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----generatebib, child='generatebib.Rnw', eval=TRUE---------------------

## ----record_start_time, echo=FALSE---------------------------------------
start.time <- proc.time()


## ----generate_bib, echo=FALSE, message=FALSE, warning=FALSE--------------
# Write all packages in the current session to a bib file
if (is.null(opts_chunk$get("bib.file"))) opts_chunk$set(bib.file="Course.bib")
write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
          file=opts_chunk$get("bib.file"))
system(paste("cat extra.bib >>", opts_chunk$get("bib.file")))
# Fix up specific issues.
# R-earth
system(paste("perl -pi -e 's|. Derived from .*$|},|'",
             opts_chunk$get("bib.file")))
# R-randomForest
system(paste("perl -pi -e 's|Fortran original by Leo Breiman",
             "and Adele Cutler and R port by|Leo Breiman and",
             "Adele Cutler and|'", opts_chunk$get("bib.file")))
# R-C50
system(paste("perl -pi -e 's|. C code for C5.0 by R. Quinlan|",
             " and J. Ross Quinlan|'", opts_chunk$get("bib.file")))
# R-caret
system(paste("perl -pi -e 's|. Contributions from|",
             " and|'", opts_chunk$get("bib.file")))
# Me
system(paste("perl -pi -e 's|Graham Williams|",
             "Graham J Williams|'", opts_chunk$get("bib.file")))





## ----file_size_weatheraus------------------------------------------------
fnwa <- file.path("data", "weatherAUS.csv")
cat(format(file.info(fnwa)$size, big.mark=","), "\n")


## ----read_csv_weatheraus-------------------------------------------------
system.time(ds <- read.csv(file=fnwa))
dim(ds)
class(ds)


## ----fread_weather-------------------------------------------------------
system.time(ds <- fread(input=fnwa))
dim(ds)
class(ds)


## ----read_csv_bigdata, eval=FALSE----------------------------------------
## ds <- read.csv(file=fnbd, nrow=1.1e6)


## ----read_csv_bigdata_strings, eval=FALSE--------------------------------
## ds <- read.csv(file=fnbd, nrow=1.1e6, stringsAsFactors=FALSE)


## ----fread_bigdata, eval=FALSE-------------------------------------------
## ds <- fread(input=fnbd, showProgress=FALSE))


## ----identify_classes----------------------------------------------------
system.time(dsf <- read.csv(file=fnwa, nrows=5e3))
classes <- sapply(dsf, class)
classes


## ----read_weather_colclases----------------------------------------------
system.time(dsf <- read.csv(file=fnwa, colClasses=classes))


## ----col_classes_bigdata, eval=FALSE-------------------------------------
## ds <- read.csv(file=fnbd, nrows=5e3)
## classes <- sapply(ds, class)
## ds <- read.csv(file=fnbd, nrows=1.1e6, colClasses=classes))
## class(ds)
## dim(ds)


## ----load_weather_datasets-----------------------------------------------
ds <- fread(input=fnwa)
setnames(ds, names(ds), normVarNames(names(ds)))


## ----dply_group, out.lines=NULL------------------------------------------
g <- ds %>% group_by(location)
g


## ----dplyr_weather_mean_rainfall-----------------------------------------
g <- g %>% summarise(daily_rainfall = mean(rainfall, na.rm=TRUE))
g


## ----dply_weather_pipeline-----------------------------------------------
ds %>% 
    group_by(location) %>% 
    summarise(daily_rainfall = mean(rainfall, na.rm=TRUE))


## ----eval=FALSE----------------------------------------------------------
## ds[WHERE, SELECT, by=GROUPBY]





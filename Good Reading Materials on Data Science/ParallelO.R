
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "GeneticProgrammingO"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))




## ----load_packages, message=FALSE----------------------------------------
library(parallel)
library(rpart)


## ----documentation, child='documentation.Rnw', eval=TRUE-----------------


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





## ----load_wsrpart, echo=FALSE, message=FALSE-----------------------------
# And load this package for the exercises.
library(wsrpart)


## ----list_csv_files------------------------------------------------------
dir(path="data", pattern="*.csv")


## ----read_csv_weather----------------------------------------------------
ds <- read.csv(file="data/weatherAUS.csv")


## ----check_weather_dataset, out.lines=5----------------------------------
dim(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)


## ------------------------------------------------------------------------
target <- "RainTomorrow"
risk   <- "RISK_MM"
dsname <- "weather"


## ----ensure_target_is_categoric------------------------------------------
ds[target] <- as.factor(ds[[target]])
summary(ds[target])


## ----identify_variables--------------------------------------------------
vars    <- colnames(ds)
ignore  <- vars[c(1, 2, if (exists("risk")) which(risk==vars))]
vars    <- setdiff(vars, ignore)
(inputs <- setdiff(vars, target))
nobs    <- nrow(ds)
dim(ds[vars])


## ----construct_the_formula-----------------------------------------------
(form <- formula(paste(target, "~ .")))


## ----set_seed------------------------------------------------------------
(seed <- sample(1:1000000, 1))
set.seed(seed)


## ----build_training_testing_datasets-------------------------------------
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))


## ----example_call_to_wsrpart---------------------------------------------
set.seed(42)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
model[[1]]$model
model[[1]]$vars
model[[1]]$accuracy


## ----example_call_to_wsrpart_repeat--------------------------------------
set.seed(84)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
model[[1]]$model
model[[1]]$vars
model[[1]]$oob.error


## ----detect_number_of_cores----------------------------------------------
cores <- detectCores()
cores


## ----run_mcparallel------------------------------------------------------
jobs <- lapply(1:cores, 
               function(x) mcparallel(wsrpart(form, ds[train,vars], ntrees=1), 
                                      name=sprintf("dt%02d", x)))


## ----inspect_processes, out.lines=NULL-----------------------------------
jobs[1:2]


## ----wait_finish---------------------------------------------------------
system.time(model <- mccollect(jobs, wait=TRUE))


## ----access_forest, out.lines=10-----------------------------------------
length(model)
model[[1]][[1]]$model
model[[2]][[1]]$model


## ----exercise_mwsrpart_cores_examples------------------------------------
system.time(model <- wsrpart(form, ds, ntrees=4, parallel=2))
num.trees <- cores
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))
model[[1]]$model


## ----exercise_mcwsrpart_timings------------------------------------------
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))


## ----make_local_cluster--------------------------------------------------
cl <- makeCluster(rep("localhost", cores))
cl


## ----local_cluster_add_3-------------------------------------------------
clusterApply(cl, 1:2, get("+"), 3)


## ----local_cluster_evalq_getwd-------------------------------------------
clusterEvalQ(cl, getwd())


## ----local_cluster_close-------------------------------------------------
stopCluster(cl)


## ----make_local_cluster_rpart--------------------------------------------
cl <- makeCluster(rep("localhost", cores))
cl


## ----local_cluster_evalq_library-----------------------------------------
clusterEvalQ(cl, {library(parallel); library(rpart); library(rattle)})


## ----local_cluster_load_dataset------------------------------------------
clusterExport(cl, c("ds", "form", "train", "vars"))


## ----local_cluster_build_rpart_model-------------------------------------
clusterExport(cl, c("varWeights", "selectVars", "wsrpart"))
system.time(model <- clusterCall(cl, wsrpart, form, ds[train, vars], ntrees=4))
length(model)


## ----local_cluster_close_rpart-------------------------------------------
stopCluster(cl)


## ----make_cluster_rpart_ms, eval=FALSE-----------------------------------
## nodes <- paste("node", 1:10, sep="")
## cl <- makeCluster(nodes)
## cl


## ----multiple_cluster_build_forest_ms, eval=FALSE------------------------
## clusterEvalQ(cl, {library(parallel); library(rpart); library(rattle)})
## clusterExport(cl,  c("varWeights", "selectVars", "wsrpart"))
## clusterExport(cl, c("varWeights", "selectVars", "wsrpart"))
## system.time(model <- clusterCall(cl, wsrpart, form, ds[train, vars], ntrees=4))


## ----multiple_cluster_close_ms, eval=FALSE-------------------------------
## stopCluster(cl)


## ----make_cluster_rpart_msmc, eval=FALSE---------------------------------
## nodes <- paste("node", 1:10, sep="")
## cl <- makeCluster(nodes)
## cl


## ----multiple_cluster_build_forest_msmc, eval=FALSE----------------------
## clusterEvalQ(cl, {library(rpart); library(rattle)})
## clusterExport(cl, "weatherDS")
## clusterExport(cl, c("varWeights", "selectVars", "wsrpart", "mcwsrpart"))
## system.time(forest <- clusterCall(cl, mcwsrpart, weatherDS, 8))


## ----multiple_cluster_close_msmc, eval=FALSE-----------------------------
## stopCluster(cl)


## ----eval=FALSE----------------------------------------------------------
## nodes <- paste("node", 2:10, sep="")
## cl <- makeCluster(nodes)
## clusterEvalQ(cl,
##              install.packages("rattle",
##                               lib="/usr/local/lib/R/site-library",
##                               repos="http://rattle.togaware.com"))
## stopCluster(cl)





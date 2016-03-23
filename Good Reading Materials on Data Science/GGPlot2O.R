## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "GGPlot2O"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))

## ----load_paccakages, message=FALSE, warning=FALSE-----------------------
library(ggplot2)   # Visualise data.
library(scales)    # Include commas in numbers.
library(RColorBrewer) # Choose different color.
library(rattle)    # Weather dataset.
library(randomForest) # Use na.roughfix() to deal with missing data.
library(gridExtra) # Layout multiple plots.
library(wq)        # Regular grid layout.
library(xkcd)      # Some xkcd fun.
library(extrafont) # Fonts for xkcd.
library(sysfonts)  # Font support for xkcd.
library(GGally)    # Parallel coordinates.
library(dplyr)     # Data wrangling.
library(lubridate) # Dates and time.

## ----common_intro, child='documentation.Rnw', eval=TRUE------------------

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



## ----prepare_dataset-----------------------------------------------------
library(rattle)
dsname <- "weatherAUS"
ds     <- get(dsname)

## ----dataset_summary-----------------------------------------------------
dim(ds)
names(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)

## ----message=FALSE-------------------------------------------------------
names(ds)  <- normVarNames(names(ds)) # Optional lower case variable names.
vars       <- names(ds)
target     <- "rain_tomorrow"
id         <- c("date", "location")
ignore     <- id
inputs     <- setdiff(vars, target)
numi       <- which(sapply(ds[vars], is.numeric))
numi
numerics   <- names(numi)
numerics
cati       <- which(sapply(ds[vars], is.factor))
cati
categorics <- names(cati)
categorics

## ----impute_missing_values-----------------------------------------------
library(randomForest)
sum(is.na(ds))
ds[setdiff(vars, ignore)] <- na.roughfix(ds[setdiff(vars, ignore)])
sum(is.na(ds))

## ----scatter, echo=FALSE, fig.height=5-----------------------------------
sobs <- sample(nrow(ds), 1000)

ds[sobs,]                                                             %>%
  ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow))            +
  geom_point()

## ----scatter, eval=FALSE-------------------------------------------------
## sobs <- sample(nrow(ds), 1000)
## 
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow))            +
##   geom_point()

## ----scatter_temp_changes_over_time, echo=FALSE--------------------------
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point()                                                         +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))

## ----scatter_temp_changes_over_time, eval=FALSE--------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point()                                                         +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))

## ----temp_changes_over_time, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point()                                                         +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time, eval=FALSE----------------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point()                                                         +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_point, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point(size=0.2)                                                 +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_point, eval=FALSE----------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point(size=0.2)                                                 +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_line, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_line()                                                          +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_line, eval=FALSE-----------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_line()                                                          +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_line_thin, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_line(size=0.1)                                                  +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----temp_changes_over_time_line_thin, eval=FALSE------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_line(size=0.1)                                                  +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))

## ----frequency_barchart, echo=FALSE--------------------------------------
p <- ds                                                               %>%
  ggplot(aes(x=wind_dir_3pm))                                          +
  geom_bar()
p

## ----frequency_barchart, eval=FALSE--------------------------------------
## p <- ds                                                               %>%
##   ggplot(aes(x=wind_dir_3pm))                                          +
##   geom_bar()
## p

## ----eval=FALSE----------------------------------------------------------
## ggsave("barchart.pdf", width=11, height=7)

## ----eval=FALSE----------------------------------------------------------
## pdf("barchart.pdf", width=11, height=7)
## p
## dev.off()

## ----bar_chart, echo=FALSE-----------------------------------------------
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm))                                          +
  geom_bar()

## ----bar_chart, eval=FALSE-----------------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm))                                          +
##   geom_bar()

## ----stacked_bar_chart, echo=FALSE, fig.height=5, fig.width=9, out.width="\\textwidth"----
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar()

## ----stacked_bar_chart, eval=FALSE---------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar()

## ----stacked_bar_chart_legend, echo=FALSE, fig.height=5, fig.width=9, out.width="\\textwidth"----
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar()                                                           +
  guides(fill=guide_legend(reverse=TRUE))

## ------------------------------------------------------------------------
levels(ds$rain_tomorrow)

## ----stacked_bar_chart_legend, eval=FALSE--------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar()                                                           +
##   guides(fill=guide_legend(reverse=TRUE))

## ----dodged_bar_chart, echo=FALSE, fig.height=5, fig.width=9, out.width="\\textwidth"----
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar(position="dodge")

## ----dodged_bar_chart, eval=FALSE----------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar(position="dodge")

## ----stacked_bar_chart_options, echo=FALSE, fig.height=5, fig.width=9, out.width="\\textwidth"----
library(scales)
library(RColorBrewer)

ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar()                                                           +
  scale_y_continuous(labels=comma)                                     +
  scale_fill_manual(values=brewer.pal(4, "Paired")[c(2,1)]             ,
                    guide=guide_legend(reverse=TRUE)                   ,
                    labels=c("No Rain Expected", "Rain Expected"))     +
  labs(title="Rain Expected by Wind Direction at 3pm"                  ,
       x=paste0("Wind Direction 3pm\n", Sys.time())                    ,
       y="Number of Days"                                              ,
       fill="Tomorrow")

## ----stacked_bar_chart_options, eval=FALSE-------------------------------
## library(scales)
## library(RColorBrewer)
## 
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar()                                                           +
##   scale_y_continuous(labels=comma)                                     +
##   scale_fill_manual(values=brewer.pal(4, "Paired")[c(2,1)]             ,
##                     guide=guide_legend(reverse=TRUE)                   ,
##                     labels=c("No Rain Expected", "Rain Expected"))     +
##   labs(title="Rain Expected by Wind Direction at 3pm"                  ,
##        x=paste0("Wind Direction 3pm\n", Sys.time())                    ,
##        y="Number of Days"                                              ,
##        fill="Tomorrow")

## ----frequency_barchart_narrow, echo=FALSE-------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=0.5)

## ----frequency_barchart_narrow, eval=FALSE-------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=0.5)

## ----frequency_barchart_wide, echo=FALSE---------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=1)

## ----frequency_barchart_wide, eval=FALSE---------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=1)

## ----frequency_barchart_wide_border, echo=FALSE--------------------------
ds                                                                    %>% 
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=1, colour="blue", fill="grey")

## ----frequency_barchart_wide_border, eval=FALSE--------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=1, colour="blue", fill="grey")

## ----frequency_barchart_colour, echo=FALSE-------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  theme(legend.position="none")

## ----frequency_barchart_colour, eval=FALSE-------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   theme(legend.position="none")

## ----frequency_barchart_colour_comma, echo=FALSE-------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  scale_y_continuous(labels=comma)                                     +
  theme(legend.position="none")

## ----frequency_barchart_colour_comma, eval=FALSE-------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   scale_y_continuous(labels=comma)                                     +
##   theme(legend.position="none")

## ----frequency_barchart_colour_dollar, echo=FALSE------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  scale_y_continuous(labels=dollar)                                    +
  labs(y="")                                                           +
  theme(legend.position="none")

## ----frequency_barchart_colour_dollar, eval=FALSE------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   scale_y_continuous(labels=dollar)                                    +
##   labs(y="")                                                           +
##   theme(legend.position="none")

## ----mean_temp3pm_location, echo=FALSE, warning=FALSE--------------------
ds                                                                    %>%
  ggplot(aes(x=location, y=temp_3pm, fill=location))                   +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")

## ----mean_temp3pm_location, eval=FALSE-----------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=location, y=temp_3pm, fill=location))                   +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")

## ----mean_temp3pm_location_rotated_labels, echo=FALSE, warning=FALSE-----
ds                                                                    %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none", 
        axis.text.x=element_text(angle=90))

## ----mean_temp3pm_location_rotated_labels, eval=FALSE--------------------
## ds                                                                    %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none",
##         axis.text.x=element_text(angle=90))

## ----mean_temp3pm_location_coord_flip, echo=FALSE, warning=FALSE---------
ds                                                                    %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")                                        +
  coord_flip()

## ----mean_temp3pm_location_coord_flip, eval=FALSE------------------------
## ds                                                                    %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")                                        +
##   coord_flip()

## ----mean_temp3pm_location_coord_flip_reorder, echo=FALSE, warning=FALSE----
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")                                        +
  coord_flip()

## ----mean_temp3pm_location_coord_flip_reorder, eval=FALSE----------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")                                        +
##   coord_flip()

## ----mean_temp3pm_location_flip_coords_ci, echo=FALSE, warning=FALSE-----
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", 
               conf.int=0.95, width=0.35)                              +
  theme(legend.position="none")                                        +
  coord_flip()

## ----mean_temp3pm_location_flip_coords_ci, eval=FALSE--------------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   stat_summary(fun.data="mean_cl_normal", geom="errorbar",
##                conf.int=0.95, width=0.35)                              +
##   theme(legend.position="none")                                        +
##   coord_flip()

## ----mean_temp3pm_location_coord_flip_labels, echo=FALSE-----------------
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, fill=location))                                 +
  geom_bar(width=1, colour="white")                                    +
  theme(legend.position="none")                                        +
  coord_flip()                                                         +
  geom_text(stat="bin", color="white", hjust=1.0, size=3,
            aes(y=..count.., label=..count..))

## ----mean_temp3pm_location_coord_flip_labels, eval=FALSE-----------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, fill=location))                                 +
##   geom_bar(width=1, colour="white")                                    +
##   theme(legend.position="none")                                        +
##   coord_flip()                                                         +
##   geom_text(stat="bin", color="white", hjust=1.0, size=3,
##             aes(y=..count.., label=..count..))

## ----mean_temp3pm_location_coord_flip_labels_commas, echo=FALSE----------
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, fill=location))                                 +
  geom_bar(width=1, colour="white")                                    +
  theme(legend.position="none")                                        +
  coord_flip()                                                         +
  geom_text(stat="bin", color="white", hjust=1.0, size=3,
            aes(y=..count.., label=scales::comma(..count..)))

## ----mean_temp3pm_location_coord_flip_labels_commas, eval=FALSE----------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, fill=location))                                 +
##   geom_bar(width=1, colour="white")                                    +
##   theme(legend.position="none")                                        +
##   coord_flip()                                                         +
##   geom_text(stat="bin", color="white", hjust=1.0, size=3,
##             aes(y=..count.., label=scales::comma(..count..)))

## ----frequency_maxtemp, echo=FALSE---------------------------------------
ds                                                                    %>%
  ggplot(aes(x=max_temp))                                              +
  geom_density()

## ----frequency_maxtemp, eval=FALSE---------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=max_temp))                                              +
##   geom_density()

## ----frequency_rainfall, echo=FALSE--------------------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_y_continuous(labels=comma)                                     +
  theme(legend.position="none")

## ----frequency_rainfall, eval=FALSE--------------------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_y_continuous(labels=comma)                                     +
##   theme(legend.position="none")

## ----frequency_rainfall_log_x, echo=FALSE--------------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_x_log10()                                                      +
  theme(legend.position="none")

## ----frequency_rainfall_log_x, eval=FALSE--------------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_x_log10()                                                      +
##   theme(legend.position="none")

## ----frequency_rainfall_log_x_breaks, echo=FALSE-------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density(binwidth=0.1)                                           +
  scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
  theme(legend.position="none")

## ----frequency_rainfall_log_x_breaks, eval=FALSE-------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density(binwidth=0.1)                                           +
##   scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
##   theme(legend.position="none")

## ----frequency_rainfall_log_x_ticks, echo=FALSE--------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density(binwidth=0.1)                                           +
  scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
  annotation_logticks(sides="bt")                                      +
  theme(legend.position="none")

## ----frequency_rainfall_log_x_ticks, eval=FALSE--------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density(binwidth=0.1)                                           +
##   scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
##   annotation_logticks(sides="bt")                                      +
##   theme(legend.position="none")

## ----frequency_rainfall_log_x_label, echo=FALSE--------------------------
mm <- function(x) { sprintf("%smm", x) }

ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_x_log10(breaks=c(1, 10, 100), labels=mm)                       +
  annotation_logticks(sides="bt")                                      +
  theme(legend.position="none")

## ----frequency_rainfall_log_x_label, eval=FALSE--------------------------
## mm <- function(x) { sprintf("%smm", x) }
## 
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_x_log10(breaks=c(1, 10, 100), labels=mm)                       +
##   annotation_logticks(sides="bt")                                      +
##   theme(legend.position="none")

## ----density_temp3pm_cities, echo=FALSE----------------------------------
cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")

ds                                                                    %>%
  filter(location %in% cities)                                        %>%
  ggplot(aes(temp_3pm, colour = location, fill = location))            +
  geom_density(alpha = 0.55)

## ----density_temp3pm_cities, eval=FALSE----------------------------------
## cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
## 
## ds                                                                    %>%
##   filter(location %in% cities)                                        %>%
##   ggplot(aes(temp_3pm, colour = location, fill = location))            +
##   geom_density(alpha = 0.55)

## ----box_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'---------
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_boxplot(notch=TRUE)                                             +
  theme(legend.position="none")

## ----box_plot, eval=FALSE------------------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_boxplot(notch=TRUE)                                             +
##   theme(legend.position="none")

## ----violin_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'------
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  theme(legend.position="none")

## ----violin_plot, eval=FALSE---------------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   theme(legend.position="none")

## ----violin_box_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'----
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  geom_boxplot(width=.5, position=position_dodge(width=0))             +
  theme(legend.position="none")

## ----violin_box_plot, eval=FALSE-----------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   geom_boxplot(width=.5, position=position_dodge(width=0))             +
##   theme(legend.position="none")

## ----violin_box_plot_location, echo=FALSE, fig.width=10, out.width='\\textwidth'----
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  geom_boxplot(width=.5, position=position_dodge(width=0))             +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1))                   +
  facet_wrap(~location)

## ----violin_box_plot_location, eval=FALSE--------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   geom_boxplot(width=.5, position=position_dodge(width=0))             +
##   theme(legend.position="none",
##         axis.text.x=element_text(angle=45, hjust=1))                   +
##   facet_wrap(~location)

## ------------------------------------------------------------------------
items <- c("Start", "IT", "Sales", "Adv", "Elect", 
           "Tax", "Salary", "Rebate", 
           "Advise", "Consult", "End")
amounts <- c(30, -5, 15, -8, -4, -7, 11, 8, -4, 20) %>% c(., sum(.))
cash <- data.frame(Transaction=factor(items, levels=items), Amount=amounts) 
cash
levels(cash$Transaction)

## ------------------------------------------------------------------------
cash %<>% mutate(Direction=ifelse(Transaction %in% c("Start", "End"), 
                   "Net", ifelse(Amount>0, "Credit", "Debit")) %>%
                 factor(levels=c("Debit", "Credit", "Net")))

## ------------------------------------------------------------------------
cash %<>% mutate(id=seq_along(Transaction),
                 end=c(head(cumsum(Amount), -1), tail(Amount, 1)),
                 start=c(0, head(end, -2), 0))

## ------------------------------------------------------------------------
cash

## ------------------------------------------------------------------------
cash %>%
  ggplot(aes(fill=Direction)) +
  geom_rect(aes(x=Transaction, 
                xmin=id-0.45, xmax=id+0.45, 
                ymin=end, ymax=start)) +
  geom_text(aes(x=id, y=start, label=Transaction), 
            size=3,
            vjust=ifelse(cash$Direction == "Debit", -0.2, 1.2)) +
  geom_text(aes(x=id, y=start, label=Amount),
            size=3, colour="white",
            vjust=ifelse(cash$Direction == "Debit", 1.3, -0.3)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Balance in Thousand Dollars") +
  ggtitle("Annual Cash Flow Analysis")

## ----ggpairs, echo=FALSE, fig.width=12, fig.height=12, out.width='\\textwidth'----
weather[c(3,4,6,7,10,19,22,24)]                                       %>%
  na.omit()                                                           %>%
  ggpairs(params=c(shape=I("."), outlier.shape=I(".")))

## ----ggpairs_echo, eval=FALSE, ref.label="ggpairs"-----------------------
## NA

## ----cumsum_plot, echo=FALSE, message=FALSE, fig.height=5----------------
cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")

ds                                                                    %>% 
  filter(location %in% cities & date >= "2009-01-01")                 %>% 
  group_by(location)                                                  %>% 
  mutate(cumRainfall=order_by(date, cumsum(rainfall)))                %>%
  ggplot(aes(x=date, y=cumRainfall, colour=location))                  +
  geom_line()                                                          +
  ylab("millimetres")                                                  +
  scale_y_continuous(labels=comma)                                     +
  ggtitle("Cumulative Rainfall")

## ----cumsum_plot, eval=FALSE---------------------------------------------
## cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")
## 
## ds                                                                    %>%
##   filter(location %in% cities & date >= "2009-01-01")                 %>%
##   group_by(location)                                                  %>%
##   mutate(cumRainfall=order_by(date, cumsum(rainfall)))                %>%
##   ggplot(aes(x=date, y=cumRainfall, colour=location))                  +
##   geom_line()                                                          +
##   ylab("millimetres")                                                  +
##   scale_y_continuous(labels=comma)                                     +
##   ggtitle("Cumulative Rainfall")

## ----parallel_coordinates, echo=FALSE, fig.height=5, out.width="\\textwidth"----
library(GGally)

cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")

ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi)                                             +
  theme(axis.text.x=element_text(angle=45))

## ----parallel_coordinates, eval=FALSE------------------------------------
## library(GGally)
## 
## cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
## 
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi)                                             +
##   theme(axis.text.x=element_text(angle=45))

## ----parallel_coordinates_hjust, echo=FALSE, fig.height=5, out.width="\\textwidth"----
ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi)                                             +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----parallel_coordinates_hjust, eval=FALSE------------------------------
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi)                                             +
##   theme(axis.text.x=element_text(angle=45, hjust=1))

## ----parallel_coordinates_group, echo=FALSE, fig.height=5, out.width="\\textwidth"----
ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi, group="location")                           +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position=c(0.25, 1),
        legend.justification=c(0.0, 1.0),
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent"),
        legend.key=element_rect(fill="transparent", 
            colour="transparent"))

## ----parallel_coordinates_group, eval=FALSE------------------------------
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi, group="location")                           +
##   theme(axis.text.x=element_text(angle=45, hjust=1),
##         legend.position=c(0.25, 1),
##         legend.justification=c(0.0, 1.0),
##         legend.direction="horizontal",
##         legend.background=element_rect(fill="transparent"),
##         legend.key=element_rect(fill="transparent",
##             colour="transparent"))

## ----polar_coords, echo=FALSE--------------------------------------------
ds                                                                    %>%
  mutate(month=format(date, "%b"))                                    %>%
  group_by(month)                                                     %>%
  summarise(max=max(max_temp))                                        %>%
  mutate(month=factor(month, levels=month.abb))                       %>%
  ggplot(aes(x=month, y=max, group=1))                                 +
  geom_bar(width=1, stat="identity", fill="orange", color="black")     +
  coord_polar(theta="x", start=-pi/12)

## ----polar_coords, eval=FALSE--------------------------------------------
## ds                                                                    %>%
##   mutate(month=format(date, "%b"))                                    %>%
##   group_by(month)                                                     %>%
##   summarise(max=max(max_temp))                                        %>%
##   mutate(month=factor(month, levels=month.abb))                       %>%
##   ggplot(aes(x=month, y=max, group=1))                                 +
##   geom_bar(width=1, stat="identity", fill="orange", color="black")     +
##   coord_polar(theta="x", start=-pi/12)

## ----multiplot_gridextra, message=FALSE----------------------------------
library(rattle)
library(ggplot2)
library(scales)

## ----dataset_subset_cities-----------------------------------------------
cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")

dss <- ds  %>% 
  filter(location %in% cities & date >= "2009-01-01") %>% 
  group_by(location) %>% 
  mutate(cumRainfall=order_by(date, cumsum(rainfall)))

## ----create_plot_p1------------------------------------------------------
p  <- ggplot(dss, aes(x=date, y=cumRainfall, colour=location))
p1 <- p + geom_line()
p1 <- p1 + ylab("millimetres")
p1 <- p1 + scale_y_continuous(labels=comma)
p1 <- p1 + ggtitle("Cumulative Rainfall")

## ----create_plot_p2------------------------------------------------------
p2 <- ggplot(dss, aes(x=date, y=max_temp, colour=location))
p2 <- p2 + geom_point(alpha=.1)
p2 <- p2 + geom_smooth(method="loess", alpha=.2, size=1)
p2 <- p2 + ggtitle("Fitted Max Temperature Curves")

## ----create_plot_p3------------------------------------------------------
p3 <- ggplot(dss, aes(x=pressure_3pm, colour=location))
p3 <- p3 + geom_density()
p3 <- p3 + ggtitle("Air Pressure at 3pm")

## ----create_plot_p4------------------------------------------------------
p4 <- ggplot(dss, aes(x=sunshine, fill=location))
p4 <- p4 + facet_grid(location ~ .)
p4 <- p4 + geom_histogram(colour="black", binwidth=1)
p4 <- p4 + ggtitle("Hours of Sunshine")
p4 <- p4 + theme(legend.position="none")

## ----plot_multiplot_gridextra, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, out.width='\\textwidth'----
library(gridExtra)
grid.arrange(p1, p2, p3, p4)

## ----plot_multiplot_gridextra, eval=FALSE--------------------------------
## library(gridExtra)
## grid.arrange(p1, p2, p3, p4)

## ----xkcd_plot, echo=FALSE, fig.height=4, warning=FALSE, message=FALSE----
library(xkcd)
xrange <- range(ds$min_temp)
yrange <- range(ds$max_temp)
ds[sobs,]                                                             %>%
  ggplot(aes(x=min_temp, y=max_temp))                                  +
  geom_point()                                                         +
  xkcdaxis(xrange, yrange) 

## ----xkcd_setup, eval=FALSE----------------------------------------------
## library(extrafont)
## download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf")
## system("mkdir ~/.fonts")
## system("mv xkcd.ttf ~/.fonts")
## font_import()
## loadfonts()

## ----xkcd_load_fonts, echo=FALSE, message=FALSE--------------------------
loadfonts()

## ----xkcd_remove_package, eval=FALSE-------------------------------------
## remove.packages(c("extrafont","extrafontdb"))

## ----xkcd_plot, eval=FALSE-----------------------------------------------
## library(xkcd)
## xrange <- range(ds$min_temp)
## yrange <- range(ds$max_temp)
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=min_temp, y=max_temp))                                  +
##   geom_point()                                                         +
##   xkcdaxis(xrange, yrange)

## ----xkcd_bar_chart, echo=FALSE, fig.height=4----------------------------
library(xkcd)
library(scales)
library(lubridate)

nds <- ds                                                             %>%
  mutate(year=year(ds$date))                                          %>%
  group_by(year)                                                      %>%
  summarise(max_temp=max(max_temp))                                   %>%
  mutate(xmin=year-0.1, xmax=year+0.1, ymin=0, ymax=max_temp)

mapping <- aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)

ggplot()                                                               +
  xkcdrect(mapping, nds)                                               +
  xkcdaxis(c(2006.8, 2014.2), c(0, 50))                                +
  labs(x="Year", y="Temperature Degrees Celcius")                      +
  scale_y_continuous(labels=comma)

## ----xkcd_bar_chart, eval=FALSE------------------------------------------
## library(xkcd)
## library(scales)
## library(lubridate)
## 
## nds <- ds                                                             %>%
##   mutate(year=year(ds$date))                                          %>%
##   group_by(year)                                                      %>%
##   summarise(max_temp=max(max_temp))                                   %>%
##   mutate(xmin=year-0.1, xmax=year+0.1, ymin=0, ymax=max_temp)
## 
## mapping <- aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)
## 
## ggplot()                                                               +
##   xkcdrect(mapping, nds)                                               +
##   xkcdaxis(c(2006.8, 2014.2), c(0, 50))                                +
##   labs(x="Year", y="Temperature Degrees Celcius")                      +
##   scale_y_continuous(labels=comma)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## # Via Tony Liu
## #
## # https://github.com/jennybc/r-graph-catalog/blob/master/r-graph-catalog/data/0003_xkcd-and-direct-labelling.R
## #
## 
## library(ggplot2)
## library(xkcd)
## 
## this_base <- "0003_xkcd-and-direct-labelling"
## 
## ## use an excerpt of the Gapminder data
## ## prepared and posted by Jenny Bryan for use in teaching
## 
## gd_url <- "http://tiny.cc/gapminder"
## gdf <- read.delim(file = gd_url)
## #gdf <- read.delim("~/gapminderDataFiveYear.txt")
## 
## str(gdf)
## 
## my_countries <- c("Australia", "China", "New Zealand", "Indonesia",
##                   "Malaysia", "Philippines", "Japan")
## 
## hdf <- droplevels(subset(gdf, country %in% my_countries))
## 
## direct_label <- data.frame(year = 2009,
##                            lifeExp = hdf$lifeExp[hdf$year == 2007],
##                            country = hdf$country[hdf$year == 2007])
## 
## xrange <- c(1952, 2014)
## yrange <- c(60, 90)
## set.seed(123)
## ratioxy<-2
## 
## mapping <- aes(x,  y,
##                scale,
##                ratioxy,
##                angleofspine ,
##                anglerighthumerus,
##                anglelefthumerus,
##                anglerightradius,
##                angleleftradius,
##                anglerightleg,
##                angleleftleg,
##                angleofneck)
## 
## dataman <- data.frame( x= c(1960), y=c(78),
##                        scale = 2,
##                        ratioxy = ratioxy,
##                        angleofspine =  -pi/2  ,
##                        anglerighthumerus = c(-pi/6),
##                        anglelefthumerus = c(-pi/2 - pi/6),
##                        anglerightradius = c(pi/5 ),
##                        angleleftradius = c(pi/5 ),
##                        angleleftleg = 3*pi/2  + pi / 12 ,
##                        anglerightleg = 3*pi/2  - pi / 12,
##                        angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10))
## 
## datalines <- data.frame(xbegin=c(1963),ybegin=c(78),  xend=c(1969), yend=c(81))
## 
## 
## p <- ggplot() +
##   geom_smooth(data = hdf,
##               aes(x = year, y = lifeExp, group = country, linetype = country),
##               se = FALSE, color = "black") +
##   theme(legend.position = "none") +
##   xkcdman(mapping, dataman) +
##   xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
##            datalines, xjitteramount = 0.12) +
##   annotate("text", x=1980, y = 85, label = "Life expectancy \nis increasing! \n But, you want to be \nliving in the right country.",family="xkcd" ) +
##   xkcdaxis(xrange, yrange) +
##   geom_text(aes(x = year + 6, y = lifeExp +1, label = country), data = direct_label,
##             hjust = 1, vjust = 1,family = "xkcd", size = 4)
## p
## 


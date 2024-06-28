## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

## install packages
library(ggpubr)
library(rstatix)
if (!require(pacman)) { install.packages("pacman") }

pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'dplyr',
               'car'
)

## ================================================================================================================
##                                                  PRE-PROCESSING
## ================================================================================================================

## read in data:
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('./data/data.csv')
d <- d[d$Finished == 1,]


## perform attention exclusions:
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
print(paste0("Number of participants hired: ", dim(d)[1]))

## Perform comprehension exclusion:
d <- subset(d, (d$comp == 3))
print(paste0("Number of participants after comprehension check: ", dim(d)[1]))

d[c('activity_1_cat', 'activity_2_cat', 'activity_3_cat', 'activity_4_cat', 'activity_5_cat')] <- ""
write.csv(d, "./data/cleaned_d.csv")

d <- read.csv("./data/rated.csv", head = TRUE, sep=";")

categories <- c(d$activity_1_category, d$activity_2_category, d$activity_3_category, d$activity_4_category, d$activity_5_category)
categories <- data.frame(counts=table(categories))
categories <- categories[order(categories$counts.Freq, decreasing=TRUE),]
write.csv(categories, "sorted_categories.csv")

## age
print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age

## gender
print(paste0("Percentage of females: ", dim(d[d$gender == 2,])[1] / dim(d)[1]))
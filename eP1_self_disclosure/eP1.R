## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2',
               'effsize',
               'tidyr',
               'dplyr',
               'lmerTest',
               'MatchIt',
               'cobalt',
               'interactions',
               'exact2x2',
               'reshape2',
               'pwr',
               'ltm',
               'splithalfr')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

d <- read.csv("./data.csv")

############################ ATTENTION AND COMPREHENSION CHECKS ############################

d <- d[d$Finished == 1, ]

# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))
print(paste0("AI Experience: ", dim(d[d$ai_companion_exp == "1",])[1] / dim(d)[1]))

# Get number of participants in all conditions
print(table(d$conditionApp))

# If conditionApp is NA, replace it with 'Journaling'
d$conditionApp <- ifelse(d$conditionApp == "", "Journaling", d$conditionApp)
d$condition <- d$conditionApp

print(table(d$condition))

######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("self_disclosure", cols)]
d[cols] <- lapply(d[cols], as.numeric)

cronbach.alpha(d[, c(
    "self_disclosure_1", 
    "self_disclosure_2")])

spearman_brown(d$self_disclosure_1, d$self_disclosure_2)

d$self_disclosure <- rowMeans(d[, c(
    "self_disclosure_1", "self_disclosure_2")])

###################### T-TESTS ######################

for (dv in c('self_disclosure', 'self_disclosure_1', 'self_disclosure_2')) { #, 'feeling_heard', 'self_disclosure'
    print(paste0("------- -*-*-*-*-*-*-* ", dv, " -*-*-*-*-*-*-* -------"))
    for (condition in c('Journaling')) {
        print(paste0("------- AI Companion vs. ", condition, " -------"))
        x <- d[d$condition == "AI Companion", dv]
        y <- d[d$condition == condition, dv]

        print(paste0("Mean x: ", round(mean(x), 2)))
        print(paste0("Mean y: ", round(mean(y), 2)))

        print(paste0("SD x: ", round(sd(x), 2)))
        print(paste0("SD y: ", round(sd(y), 2)))

        # Print sample size
        print(paste0("Sample size x: ", length(x)))
        print(paste0("Sample size y: ", length(y)))

        ttest <- t.test(x, y, paired = FALSE)
        print(ttest)

        cd <- cohen.d(x, y, paired = FALSE)
        print(cd)
    }
}

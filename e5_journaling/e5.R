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
               'ltm')

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

# Rename Rudimentary to Control
d$condition <- ifelse(d$condition == "Rudimentary", "Control", d$condition)

table(d$journaling_manip)

######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("lonely", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Reverse-code negative items, i.e., lonely_after_10, lonely_after_14, lonely_after_16, lonely_after_19
d$lonely_after_10 <- 100 - d$lonely_after_10
d$lonely_after_14 <- 100 - d$lonely_after_14
d$lonely_after_16 <- 100 - d$lonely_after_16
d$lonely_after_19 <- 100 - d$lonely_after_19


# Now aggregate ratings for loneliness, lighting, temperature, sound, UX, self-disclosure, and feeling heard
cronbach.alpha(d[, c(
    "lonely_after_1", 
    "lonely_after_2", 
    "lonely_after_3")])

d$lonely_after <- rowMeans(d[, c(
    "lonely_after_1", 
    "lonely_after_2", 
    "lonely_after_3")])

d$lighting <- rowMeans(d[, c(
    "lonely_after_9", 
    "lonely_after_10")])

d$temperature <- rowMeans(d[, c(
    "lonely_after_13", 
    "lonely_after_14")])

d$sound <- rowMeans(d[, c(
    "lonely_after_15", 
    "lonely_after_16")])

d$ux <- rowMeans(d[, c(
    "lonely_after_18", 
    "lonely_after_19")])

# Correlation table with all the variables
cor(d[, c("lonely_after", "lighting", "temperature", "sound", "ux")])

###################### RELIABILITY ######################

library(semTools)

# Now test discriminant validity with HTMT ratio
HS.model <- 'lonely_after =~ lonely_after_1 + lonely_after_2 + lonely_after_3
                lighting =~ lonely_after_9 + lonely_after_10
                temperature =~ lonely_after_13 + lonely_after_14
                sound =~ lonely_after_15 + lonely_after_16
                ux =~ lonely_after_18 + lonely_after_19'

htmt(HS.model, data = d)

#########################################################

# Print all ratings that we calculated above for all conditions
d %>% group_by(condition) %>% summarise(mean_lonely_after = mean(lonely_after), 
                                        mean_lighting = mean(lighting),
                                        mean_temperature = mean(temperature),
                                        mean_sound = mean(sound),
                                        mean_ux = mean(ux))

###################### ANOVA ######################

# First, we will an ANOVA comparing loneliness after interaction by condition
anova_loneliness <- aov(lonely_after ~ condition, data = d)
summary(anova_loneliness)

TukeyHSD(anova_loneliness)

for (dv in c('lonely_after')) {
    print(paste0("------- -*-*-*-*-*-*-* ", dv, " -*-*-*-*-*-*-* -------"))
    for (condition in c('Control', 'Journaling')) {
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


# journaling vs. control
print(paste0("------- -*-*-*-*-*-*-* Journaling vs. Control -*-*-*-*-*-*-* -------"))
x <- d[d$condition == "Journaling", 'lonely_after']
y <- d[d$condition == "Control", 'lonely_after']

print(paste0("Mean x: ", round(mean(x), 2)))
print(paste0("Mean y: ", round(mean(y), 2)))
print(paste0("SD x: ", round(sd(x), 2)))
print(paste0("SD y: ", round(sd(y), 2)))

t.test(x, y, paired = FALSE)


# Check presence of the string "lone" occurance in study_purpose
loneliness_keywords <- "lone|isolation|friend|companion"
d$study_purpose_lone <- ifelse(grepl(loneliness_keywords, d$study_purpose, ignore.case = TRUE), 1, 0)

# Get explanations with study_purpose_lone == 1
dim(d[d$study_purpose_lone == 1, c('study_purpose', 'worker_id')])

purpose <- read.csv("study_purpose.csv", sep = ";", header = TRUE)
purpose <- purpose[purpose$purpose_correct == 1, ]

# Uncomment to exclude those who have correctly identified the study purpose
#d <- d[!(d$worker_id %in% purpose$worker_id), ]
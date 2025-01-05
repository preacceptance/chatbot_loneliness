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
               'BayesFactor')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

d <- read.csv("./data.csv")

############################ ATTENTION AND COMPREHENSION CHECKS ############################

d <- d[d$Finished == 1, ]

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("self_disclosure", cols)]
d[cols] <- lapply(d[cols], as.numeric)

d$distraction_1 <- d$self_disclosure_1
d$distraction_2 <- d$self_disclosure_2
d$distraction_3 <- d$self_disclosure_4
d$distraction_4 <- d$self_disclosure_5
d$distraction_5 <- d$self_disclosure_6
d$distraction_6 <- d$self_disclosure_7


d <- d[complete.cases(d[, c(
    "distraction_1", 
    "distraction_2",
    "distraction_3",
    "distraction_4",
    "distraction_5",
    "distraction_6")]),]

dim(d)

d <- d[!is.na(d$condition),]

# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))

table(d$condition)

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))
print(paste0("AI Experience: ", dim(d[d$ai_companion_exp == "1",])[1] / dim(d)[1]))


# Now aggregate ratings for loneliness, lighting, temperature, sound, UX, self-disclosure, and feeling heard
cronbach.alpha(d[, c(
    "distraction_1", 
    "distraction_2",
    "distraction_3",
    "distraction_4",
    "distraction_5",
    "distraction_6")])

d$distraction <- rowMeans(d[, c(
    "distraction_1", 
    "distraction_2",
    "distraction_3",
    "distraction_4",
    "distraction_5",
    "distraction_6")])

# anova
aov_res <- aov(distraction ~ condition, data = d)
summary(aov_res)

# post-hoc
TukeyHSD(aov_res)

###################### T-TESTS ######################

# Groupby means
d %>% group_by(condition) %>% summarise(mean_distraction = mean(distraction))

for (dv in c('distraction')) { #, 'feeling_heard', 'self_disclosure'
    print(paste0("------- -*-*-*-*-*-*-* ", dv, " -*-*-*-*-*-*-* -------"))
    for (condition in c('youtube', 'journaling')) {
        print(paste0("------- AI chatbot vs. ", condition, " -------"))
        x <- d[d$condition == "AI chatbot", dv]
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

x <- d[d$condition == "AI chatbot", dv]
y <- d[d$condition == "journaling", dv]

result <- 1 / ttestBF(x, y)
print(result)
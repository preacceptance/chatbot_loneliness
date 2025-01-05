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

d <- read.csv("./data/data.csv")

############################ ATTENTION AND COMPREHENSION CHECKS ############################
# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))


# Exclude participants based on comprehension check for all 3 conditions: 'Control', 'Prediction', 'Experience'
size_before <- dim(d)[1]
d <- d[d$comp_1 == "1" & d$comp_2 == "1",]
print(paste0("Exclusions from comprehension check: ", size_before - dim(d)[1]))

dim(d)

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))
print(paste0("AI Experience: ", dim(d[d$ai_companion_exp == "1",])[1] / dim(d)[1]))

# Get number of participants in both experience and control conditions
print(table(d$condition))

######### Prepare Data #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("lone", cols) | grepl("comp", cols) | grepl("perf_", cols) | grepl("heard_", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Rename condition names:
# "rudimentary" -> Control
# "generalist_ai" -> AI Assistant
# "regular" -> AI Companion
d$condition <- ifelse(d$condition == "rudimentary", "Control", d$condition)
d$condition <- ifelse(d$condition == "regular", "AI Companion", d$condition)

d$lonely_after <- rowMeans(d[, c(
    "lonely_after_1", 
    "lonely_after_2", 
    "lonely_after_3")])

# Cronbach's alpha for loneliness ratings
cronbach.alpha(d[, c("lonely_after_1", "lonely_after_3", "lonely_after_3")])

d %>% group_by(condition) %>% summarise(mean_loneliness_after = mean(lonely_after))

###################### T-TESTS ######################

# Compare only after loneliness ratings
for (condition in c('Control')) {
    print(paste0("------- AI Companion vs. ", condition, " -------"))
    x <- d[d$condition == "AI Companion", 'lonely_after']
    y <- d[d$condition == condition, 'lonely_after']

    print(paste0("Mean x: ", round(mean(x), 2)))
    print(paste0("Mean y: ", round(mean(y), 2)))

    # Print sample size
    print(paste0("Sample size x: ", length(x)))
    print(paste0("Sample size y: ", length(y)))

    ttest <- t.test(x, y, paired = FALSE)
    print(ttest)

    cd <- cohen.d(x, y, paired = FALSE)
    print(cd)
}

###################### PLOTTING ######################

# Create a long format of the data, i.e., before and after loneliness ratings in separate rows
d_long <- melt(d, id.vars = c('condition', 'worker_id'), measure.vars = c('lonely_after'))

# Rename lonely_before as before and lonely_after as after.
d_long$condition <- factor(d_long$condition, levels = c('AI Companion', 'Control'))
# Then change 'variable' column name to 'lonely'
d_long <- d_long %>% rename(lonely = variable)

plotter <- function(y_var='value', title='') {
    p1 <- ggplot(d_long, aes_string(x = 'condition', y = y_var, fill = 'lonely')) +
        geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 1, size = 0.75) +
        stat_summary(fun.data = "mean_cl_boot", color = "black",
                size = 0.4, 
                position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                position = position_dodge(width = 0.9),
                geom = "errorbar", width = 0.2) +
        
        theme_bw() +
        theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        ggtitle(title) +
        scale_fill_manual(values = c("#cccccc", "#666666"), name = "Timing Relative to Experience:", guide = guide_legend(reverse = FALSE)) +
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 16)) +
        theme(axis.text.y = element_text(size = 16)) +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18))

    return(p1)
}

p1 <- plotter()
ggsave("plot.pdf", last_plot(), dpi = 500, width = 10, height = 6)

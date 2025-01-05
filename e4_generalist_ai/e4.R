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

print(paste0("Number of participants after exclusions: ", dim(d)[1]))

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))
print(paste0("AI Experience: ", dim(d[d$ai_companion_exp == "1",])[1] / dim(d)[1]))

# Get number of participants in both experience and control conditions
print(table(d$condition))

# Rename column generalist_ai_v3 to generalist_ai
d$condition <- ifelse(d$condition == "generalist_ai_v3", "generalist_ai", d$condition)

######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("lone", cols) | grepl("comp", cols) | grepl("perf_", cols) | grepl("heard_", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Rename condition names:
# "rudimentary" -> Control
# "generalist_ai" -> AI Assistant
# "regular" -> AI Companion
d$condition <- ifelse(d$condition == "rudimentary", "Control", d$condition)
d$condition <- ifelse(d$condition == "generalist_ai", "AI Assistant", d$condition)
d$condition <- ifelse(d$condition == "regular", "AI Companion", d$condition)


# Now aggregate all 3 loneliness ratings into 1, by taking the mean
d$lonely_before <- rowMeans(d[, c(
    "lonely_before_1", 
    "lonely_before_2", 
    "lonely_before_3")])

d$lonely_after <- rowMeans(d[, c(
    "lonely_after_1", 
    "lonely_after_2", 
    "lonely_after_3")])

# Cronbach's alpha for loneliness ratings
cronbach.alpha(d[, c("lonely_before_1", "lonely_before_2", "lonely_before_3")])
cronbach.alpha(d[, c("lonely_after_1", "lonely_after_3", "lonely_after_3")])

# Cronbach's alpha for performance ratings
cronbach.alpha(d[, c("perf_1_1", "perf_2_1", "perf_3_1", "perf_4_1", "perf_5_1")])

# Cronbach's alpha for feeling heard ratings
cronbach.alpha(d[, c("heard_1_1", "heard_2_1", "heard_3_1")])

# Calculate difference between loneliness ratings for each of the 3 questions
d$lonely_diff_1 <- d$lonely_before_1 - d$lonely_after_1
d$lonely_diff_2 <- d$lonely_before_2 - d$lonely_after_2
d$lonely_diff_3 <- d$lonely_before_3 - d$lonely_after_3

# Print all three loneliness difference ratings for all 3 conditions
d %>% group_by(condition) %>% summarise(mean_diff_1 = mean(lonely_diff_1), 
                                        mean_diff_2 = mean(lonely_diff_2),
                                        mean_diff_3 = mean(lonely_diff_3),
                                        sd_diff_1 = sd(lonely_diff_1),
                                        sd_diff_2 = sd(lonely_diff_2),
                                        sd_diff_3 = sd(lonely_diff_3))

# Do the same for heard_1_1, heard_2_1, heard_3_1
d$heard <- rowMeans(d[, c(
    "heard_1_1", 
    "heard_2_1",
    "heard_3_1"
)])

# Do the same for perf_1_1, perf_2_1, perf_3_1, perf_4_1, perf_5_1
d$perf <- rowMeans(d[, c(
    "perf_1_1", 
    "perf_2_1",
    "perf_3_1",
    "perf_4_1",
    "perf_5_1"
)])

# Print mean performance questions for all 3 conditions
d %>% group_by(condition) %>% summarise(mean_perf_1 = mean(perf_1_1), 
                                        mean_perf_2 = mean(perf_2_1),
                                        mean_perf_3 = mean(perf_3_1),
                                        mean_perf_4 = mean(perf_4_1),
                                        mean_perf_5 = mean(perf_5_1))

###################### RESULTS ######################

# Calculate loneliness reduction (difference between before and after)
d$loneliness_reduction <- d$lonely_before - d$lonely_after

# First, we will run paired t-tests comparing loneliness before vs. after interaction in all 3 conditions
for (condition in c('Control', 'AI Assistant', 'AI Companion')) {
    print(paste0("------- Condition: ", condition, " -------"))
    x <- d[d$condition == condition, 'lonely_before']
    y <- d[d$condition == condition, 'lonely_after']

    print(paste0("Mean x: ", round(mean(x), 2)))
    print(paste0("Mean y: ", round(mean(y), 2)))

    ttest <- t.test(x, y, paired = TRUE)
    print(ttest)

    cd <- cohen.d(x, y, paired = TRUE)
    print(cd)
}


### Print mean and SD's ###
# For loneliness before and after
for (var in c('lonely_before', 'lonely_after')) {
    print(paste0("------- ", var, " -------"))
    means <- round(tapply(d[, var], d$condition, mean), 2)
    sds <- round(tapply(d[, var], d$condition, sd), 2)
    
    for (condition in names(means)) {
        print(paste0("Condition: ", condition, " - Mean: ", means[condition], ", SD: ", sds[condition]))
    }
}

# For all three conditions, for loneliness decrease, feeling heard, and performance
for (var in c('loneliness_reduction', 'heard', 'perf')) {
    print(paste0("------- ", var, " -------"))
    means <- round(tapply(d[, var], d$condition, mean), 2)
    sds <- round(tapply(d[, var], d$condition, sd), 2)
    
    for (condition in names(means)) {
        print(paste0("Condition: ", condition, " - Mean: ", means[condition], ", SD: ", sds[condition]))
    }
}

# Run a one-way ANOVA for loneliness reduction
anova_loneliness <- aov(loneliness_reduction ~ condition, data = d)
summary(anova_loneliness)

# Tukey's Honest Significant Difference (HSD) test
TukeyHSD(anova_loneliness)

# Feeling heard ---

anova_feeling_heard <- aov(heard ~ condition, data = d)
summary(anova_feeling_heard)

TukeyHSD(anova_feeling_heard)

# Performance ---

anova_perf <- aov(perf ~ condition, data = d)
summary(anova_perf)

TukeyHSD(anova_perf)

###################### PLOTTING ######################


plotter <- function(y_var='value', title='') {
    p1 <- ggplot(d_long, aes_string(x = 'condition', y = y_var)) + #, fill = 'lonely'
        geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 1, size = 0.75) +
        stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
        stat_summary(fun.data = "mean_cl_boot", color = "black",
                    position = position_dodge(width = 0.9)) +
        theme_bw() +
        theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        ggtitle(title) +
        #scale_fill_manual(values = c("#cccccc", "#666666"), name = "Timing Relative to Experience:", guide = guide_legend(reverse = FALSE)) +
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 16)) +
        theme(axis.text.y = element_text(size = 16)) +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18))

    return(p1)
}

d_long <- melt(d, id.vars = c('condition', 'worker_id'), measure.vars = c('lonely_before', 'lonely_after'))
p1 <- plotter()
ggsave("plots/plot.pdf", last_plot(), dpi = 500, width = 10, height = 6)

# Also plot feeling heard and performance (no pre-post there, just mean values)
d_long <- melt(d, id.vars = c('condition', 'worker_id'), measure.vars = c('heard'))
p2 <- plotter(title='Feeling Heard')
ggsave("plots/plot_heard.pdf", last_plot(), dpi = 500, width = 5, height = 6)

d_long <- melt(d, id.vars = c('condition', 'worker_id'), measure.vars = c('perf'))
p3 <- plotter(title='Performance')
ggsave("plots/plot_perf.pdf", last_plot(), dpi = 500, width = 5, height = 6)

###################### LONELINESS REDUCTION AND BASELINE LONELINESS LEVELS ######################

# Create a long format of the data, i.e., before and after loneliness ratings in separate rows
d_long <- melt(d, id.vars = c('condition', 'worker_id'), measure.vars = c('lonely_before', 'lonely_after'))

# Rename lonely_before as before and lonely_after as after.
d_long$variable <- ifelse(d_long$variable == "lonely_before", "Pre", "Post")
d_long$variable <- factor(d_long$variable, levels = c("Pre", "Post"))
d_long$condition <- factor(d_long$condition, levels = c('Control', 'AI Assistant', 'AI Companion'))
# Then change 'variable' column name to 'lonely'
d_long <- d_long %>% rename(lonely = variable)

# Now, calculate the effect sizes for different loneliness levels
# For this, divide the data into two groups: lonely and not lonely. 
# To do this, we have several constant loneliness thresholds ranging from 20 to 80.
# Then, we will calculate the effect size for each group
loneliness_thresholds <- (0:100)

d_long_exp <- d_long %>%
  filter(condition == "AI Companion")  # Only use AI Companion condition

# Initialize effect_sizes as an empty list
effect_sizes <- list()
loneliness_diffs <- list()

before <- d_long_exp[d_long_exp$lonely == "Pre", 'value']
after <- d_long_exp[d_long_exp$lonely == "Post", 'value']

for(threshold in loneliness_thresholds) {
    lonely_before <- before[before > threshold]
    lonely_after <- after[before > threshold]  # Use 'before > threshold' to index 'after' as well

    if (length(lonely_before) < 10) {
        next
    }

    # Perform the t-test
    vart <- var.test(lonely_before, lonely_after)
    tt <- t.test(lonely_before, lonely_after, paired = TRUE, var.equal = vart$p.value > 0.05)

    # Calculate and store the effect size
    effect_sizes[[as.character(threshold)]] <- cohen.d(lonely_before, lonely_after, paired=TRUE)$estimate
    loneliness_diffs[[as.character(threshold)]] <- mean(lonely_before) - mean(lonely_after)
}


# Plot this, with threshold in x-axis and effect size in y-axis
effect_sizes_df <- data.frame(threshold = as.numeric(names(effect_sizes)), effect_size = unlist(effect_sizes))
loneliness_diffs_df <- data.frame(threshold = as.numeric(names(loneliness_diffs)), loneliness_diff = unlist(loneliness_diffs))

plot <- ggplot(data = effect_sizes_df, aes(x = threshold, y = effect_size)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme_classic() +
    labs(x = "Loneliness Threshold",
         y = "Effect Size (Cohen's d)") +
    ylim(0.2, 1.2) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 0, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "top", legend.direction = "horizontal")

ggsave("./plots/effect_sizes.pdf", plot, width = 8, height = 6, dpi = 300)

model <- lm(effect_size ~ threshold, data = effect_sizes_df)
summary(model)

###################### MEDIATION ######################

# Fifth, we will run a mediation model with AI companion/AI assistant/control as a multicategorial IV, feeling heard and performance as mediators, and loneliness reduction as DV
source("../process.R")

d$condition_numeric <- ifelse(d$condition == "AI Companion", 1, ifelse(d$condition == "AI Assistant", 2, 3))
d$loneliness_reduction <- d$lonely_before - d$lonely_after

process(data = d, y = "loneliness_reduction", x = "condition_numeric",
        m = c("heard", "perf"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

### Test following serial model: IV -> feeling heard -> performance -> loneliness reduction
process(data = d, y = "loneliness_reduction", x = "condition_numeric",
        m = c("heard", "perf"), model = 6, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

### For contrast, do not look at multiple conditions as mcx is not supported. Instead, look at the conditions separately
d_1_2 <- d[d$condition %in% c("AI Companion", "AI Assistant"),]
process(data = d_1_2, y = "loneliness_reduction", x = "condition_numeric",
        m = c("heard", "perf"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000 , modelbt = 1, seed = 654321)

d_1_3 <- d[d$condition %in% c("AI Companion", "Control"),]
process(data = d_1_3, y = "loneliness_reduction", x = "condition_numeric",
        m = c("heard", "perf"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000 , modelbt = 1, seed = 654321)

# Test correlation between feeling heard and performance
cor.test(d$heard, d$perf)
library(semTools)

# Now test discriminant validity with HTMT ratio
HS.model <- 'heard =~ heard_1_1 + heard_2_1 + heard_3_1
            perf =~ perf_1_1 + perf_2_1 + perf_3_1 + perf_4_1 + perf_5_1'

htmt(HS.model, data = d)
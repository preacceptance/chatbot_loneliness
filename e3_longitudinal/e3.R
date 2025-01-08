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
               'ltm', 'boot', 'simr')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

DAYS <- c(2, 3, 4, 5, 6, 7)

d <- read.csv("data/data_day1.csv")

############################ ATTENTION AND COMPREHENSION CHECKS ############################
# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))

# Exclude participants based on comprehension check for all 3 conditions: 'Control', 'Prediction', 'Experience'
size_before <- dim(d)[1]
d$comp_prediction <- d$comp_1_p == "1" & d$comp_2_p == "1"
d$comp_control <- d$comp_1_c == "1" & d$comp_2_c == "1"
d$comp_experience <- d$comp_1_e == "1" & d$comp_2_e == "1"

d$passed_comp_check <- d$comp_prediction | d$comp_control | d$comp_experience
d <- d[d$passed_comp_check == 1,]
d$day <- 1
print(paste0("Exclusions from comprehension check: ", size_before - dim(d)[1]))

# Remove row with worker_id='0D7E358BFF384C8EB131A4BE14C08036' on day 1 with condition 'Control', since this worker_id is a duplicate
d <- d[!(d$worker_id == '0D7E358BFF384C8EB131A4BE14C08036' & d$day == 1 & d$condition == "Control"),]

# Get number of participants in both experience and control conditions
print(table(d$condition))
sample_before <- table(d$condition)

print(paste0("Age: ", mean(as.numeric(d[d$day == 1,]$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", sum(d[d$day == 1, 'gender'] == "2")[1] / dim(d[d$day == 1,])[1]))

############################ AGGREGATE RATINGS FROM ALL DAYS ############################
GET_ONLY_FINISHED <- TRUE  # If TRUE, only include participants who finished the study, i.e., completed all 7 days

for (DAY in DAYS) {
    if(DAY == 1) { next } # We already read day 1

    # Load data from specific day
    d_day <- read.csv(paste0("data/data_day", DAY, ".csv"))
    d_day <- d_day[d_day$Finished == 1,]
    d_day$day <- DAY

    # Remove rows with NA worker_id
    d_day <- d_day[!is.na(d_day$worker_id),]

    # Check duplicate worker_ids
    if(length(d_day$worker_id) != length(unique(d_day$worker_id))) {
        # Remove duplicates
        d_day <- d_day[!duplicated(d_day$worker_id),]
    }

    # Print number of participants on Day X
    print(paste0("Number of participants on Day ", DAY, ": ", dim(d_day)[1]))
    
    # Combine data with other days. Ignore missing columns
    d <- dplyr::bind_rows(d, d_day)

    if(DAY == DAYS[length(DAYS)]) { # If this is the last day
        # Collect worker ids on each day in vectors
        if(GET_ONLY_FINISHED) {
            ids_day1 <- d[d$day == 1, 'worker_id']
            ids_day2 <- d[d$day == 2, 'worker_id']
            ids_day3 <- d[d$day == 3, 'worker_id']
            ids_day4 <- d[d$day == 4, 'worker_id']
            ids_day5 <- d[d$day == 5, 'worker_id']
            ids_day6 <- d[d$day == 6, 'worker_id']
            ids_day7 <- d[d$day == 7, 'worker_id']
            
            # Find intersection of worker ids between all days
            common_ids <- intersect(ids_day1, ids_day2)
            common_ids <- intersect(common_ids, ids_day3)
            common_ids <- intersect(common_ids, ids_day4)
            common_ids <- intersect(common_ids, ids_day5)
            common_ids <- intersect(common_ids, ids_day6)
            common_ids <- intersect(common_ids, ids_day7)
            common_ids <- c(common_ids, d[d$day == 1 & d$condition == "Prediction", 'worker_id']) # Also include the prediction condition

            d <- d[(d$worker_id %in% common_ids), ] # Keep only the common worker ids and prediction condition from day 1
        }
        
        # Remove rows with NA worker_id just in case
        d <- d[!is.na(d$worker_id),]
    }
}

print(paste0("Number of participants who finished the study: ", length(unique(d$worker_id))))
print(table(d[, c('condition', 'day')]))

# Attrition rates:
attrition_control <- sample_before['Control'] - table(d[d$condition == "Control", 'day'])[1]
attrition_experience <- sample_before['Experience'] - table(d[d$condition == "Experience", 'day'])[1]

# Compare attrition rates between conditions
print("Attrition rates:")
print(prop.test(c(attrition_control, attrition_experience), c(sample_before['Control'], sample_before['Experience'])))

print(paste0("Age: ", mean(as.numeric(d[d$day == 1,]$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", sum(d[d$day == 1, 'gender'] == "2")[1] / dim(d[d$day == 1,])[1]))
print(paste0("AI Experience: ", sum(d[d$day == 1, 'ai_companion_exp'] == "1")[1] / dim(d[d$day == 1,])[1]))

# Calculate mean duration in seconds in each condition
print(paste0("Mean duration in minutes in Control condition: ", mean(as.numeric(d[d$condition == "Control", 'Duration..in.seconds.']), na.rm = TRUE) / 60))
print(paste0("Mean duration in minutes in Experience condition: ", mean(as.numeric(d[d$condition == "Experience", 'Duration..in.seconds.']), na.rm = TRUE) / 60))

############## PREPARING THE DATASET ##############

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("lone", cols) | grepl("comp", cols)]
d[cols] <- lapply(d[cols], as.numeric)

dim(d)

# Now aggregate all 3 loneliness ratings into 1, by taking the mean
d$control_lonely <- rowMeans(d[, c(
    "control_loneliness_1", 
    "control_loneliness_2", 
    "control_loneliness_3")])

l <- mutate_all(d[d$condition == 'Control', c('control_loneliness_1', 'control_loneliness_2', 'control_loneliness_3')], function(x) as.numeric(as.character(x)))
cronbach.alpha(l, na.rm = TRUE)

d$exp_lonely_before <- rowMeans(d[, c(
    "exp_lonely_before_1", 
    "exp_lonely_before_2", 
    "exp_lonely_before_3")])

l <- mutate_all(d[d$condition == 'Experience', c('exp_lonely_before_1', 'exp_lonely_before_2', 'exp_lonely_before_3')], function(x) as.numeric(as.character(x)))
cronbach.alpha(l, na.rm = TRUE)

d$exp_lonely_after <- rowMeans(d[, c(
    "exp_lonely_after_1", 
    "exp_lonely_after_2", 
    "exp_lonely_after_3")])

l <- mutate_all(d[d$condition == 'Experience', c('exp_lonely_after_1', 'exp_lonely_after_2', 'exp_lonely_after_3')], function(x) as.numeric(as.character(x)))
cronbach.alpha(l, na.rm = TRUE)

# Now do the same for all 7 days in prediction condition (X1-X7)
for (i in 1:7) {
    for (before_after in c("before", "after")) {
        d[[paste0("pred_lonely_", before_after, "_day", i)]] <- rowMeans(d[, c(
            paste0("X", i, "_lonely_1_", before_after, "_1"), 
            paste0("X", i, "_lonely_1_", before_after, "_2"),
            paste0("X", i, "_lonely_1_", before_after, "_3")
        )])
    }
}

colnames(d)

################## FLATTEN THE DATASET ##################

d_plot <- d[, c('worker_id', 'condition', 'day', 'control_lonely', 'exp_lonely_before', 'exp_lonely_after', paste0("pred_lonely_before_day", 1:7), paste0("pred_lonely_after_day", 1:7))]

# Create a new dataframe for plotting with the following columns: 'worker_id', 'condition', 'day', 'timepoint' (before/after), 'loneliness'
# i.e., flatten the data

d_long <- d_plot %>%
    pivot_longer(cols = c('control_lonely', 'exp_lonely_before', 'exp_lonely_after', paste0("pred_lonely_before_day", 1:7), paste0("pred_lonely_after_day", 1:7)),
                 names_to = "timepoint",
                 values_to = "loneliness")

# Remove rows in which 'loneliness' is NA
d_long <- d_long[!is.na(d_long$loneliness),]

substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

############ MERGE ENGAGEMENT DATA ############

# Merge engagement data with d_long
engagement <- read.csv("data/n_messages.csv")

# For prediction condition, we need to extract the day from the column name which is the last character of the 'timepoint' column name.
d_long$day <- ifelse(d_long$condition == "Prediction", 
                                substrRight(d_long$timepoint, 1),
                                d_long$day)

# Now, edit the timepoint column. If it contains 'before', set it to 'before', if it contains 'after', set it to 'after', otherwise set it to 'NA'
d_long$timepoint <- ifelse(grepl("before", d_long$timepoint), "before", 
                                ifelse(grepl("after", d_long$timepoint), "after", "before"))

# Now, merge the engagement data with d_long_diff on both worker_id and day. We want to still keep rows that doesn't exist in engagement data
d_long_engagement <- merge(d_long, engagement, by = c("worker_id", "day"), all.x = TRUE)

############ PLOT LONELINESS SCORES OVER 7 DAYS ############

# Now, plot loneliness scores over all days, for all conditions. We should have 5 lines in total: control, exp_before, exp_after, pred_before, pred_after
# We should use the 'day' column for all conditions except for the prediction condition, where we should use 'day' and 'timepoint' columns
ggplot(data = d_long, aes(x = day, y = loneliness, color = condition, fill = condition, group = interaction(condition, timepoint), linetype = timepoint)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    # Add 95% confidence intervals
    stat_summary(fun.data = mean_cl_normal, fun.args=list(conf.int=0.95), geom = "ribbon", alpha = 0.04, colour = NA) +
    scale_fill_manual(values = c("Control" = "green", "Experience" = "blue", "Prediction" = "white")) +
    scale_color_manual(values = c("Control" = "green", "Experience" = "blue", "Prediction" = "white")) +
    theme_minimal() +
    theme_classic() +
    labs(title = "",
         x = "Day",
         y = "Loneliness Score",
         color = "Group",
         linetype = "Timepoint") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 0, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "top", legend.direction = "horizontal")

# Save the plot in a pdf file
ggsave("plots/loneliness_scores_over_7_days_1.pdf", width = 8, height = 6)

# Now, plot loneliness scores over all days, for all conditions. We should have 5 lines in total: control, exp_before, exp_after, pred_before, pred_after
# We should use the 'day' column for all conditions except for the prediction condition, where we should use 'day' and 'timepoint' columns
ggplot(data = d_long, aes(x = day, y = loneliness, color = condition, fill = condition, group = interaction(condition, timepoint), linetype = timepoint)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    # Add 95% confidence intervals
    stat_summary(fun.data = mean_cl_normal, fun.args=list(conf.int=0.95), geom = "ribbon", alpha = 0.04, colour = NA) +
    scale_fill_manual(values = c("Control" = rgb(1, 1, 1, 0), "Experience" = "blue", "Prediction" = "red")) +
    scale_color_manual(values = c("Control" = rgb(1, 1, 1, 0), "Experience" = "blue", "Prediction" = "red")) +
    theme_minimal() +
    theme_classic() +
    labs(title = "",
         x = "Day",
         y = "Loneliness Score",
         color = "Group",
         linetype = "Timepoint") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 0, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "top", legend.direction = "horizontal")

# Save the plot in a pdf file
ggsave("plots/loneliness_scores_over_7_days_2.pdf", width = 8, height = 6)


# Print mean loneliness before and after in all conditions, to check if the plot is correct
d_summary <- d_long %>%
    group_by(condition, timepoint, day) %>%
    summarise(mean_loneliness = mean(loneliness))

print(d_summary, n=300)

###################### PLOT ENGAGEMENT OVER 7 DAYS ######################

d_long_exp_eng <- d_long_engagement %>%
  filter(condition == "Experience")

d_long_exp_eng_melt <- melt(d_long_exp_eng, id.vars = c("day", "condition"), measure.vars = c("n_messages", "avg_n_words"))

# Plot the data
ggplot(data = d_long_exp_eng_melt, aes(x = day, y = value, color = condition, group = interaction(condition, variable))) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    scale_color_manual(values = c("Control" = "green", "Experience" = "blue", "Prediction" = "red")) +
    facet_wrap(~ variable, scales = "free_y") +
    theme_minimal() +
    theme_classic() +
    labs(title = "",
         x = "Day",
         y = "Value",
         color = "Group") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 0, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "top", legend.direction = "horizontal")

# Calculate difference between loneliness scores before and after for all conditions.
d_long_diff <- d_long

# For each worker_id, calculate the difference between 'before' and 'after' loneliness scores
d_long_diff <- d_long_diff %>%
    group_by(worker_id, condition, day) %>%
    mutate(diff = lag(loneliness) - loneliness) %>%
    ungroup()

# Merge engagement scores with d_long_diff
d_long_diff <- merge(d_long_diff, engagement, by = c("worker_id", "day"), all.x = TRUE)

# Remove all rows where 'diff' is NA
d_long_diff <- d_long_diff[!is.na(d_long_diff$diff),]

# Remove timepoint column
d_long_diff <- d_long_diff[, c('worker_id', 'condition', 'day', 'diff', 'n_messages', 'avg_n_words')]

###################### RESULTS ######################

d_long_exp <- d_long %>%
  filter(condition == "Experience")

d_long_exp$timepoint <- as.numeric(as.factor(d_long_exp$timepoint))
d_long_exp$day <- as.numeric(as.factor(d_long_exp$day))
model <- lmer(loneliness ~ day * timepoint + (1|worker_id), data = d_long_exp)
summary(model)

#### as loneliness before interaction was significantly higher than loneliness after interaction when we aggregated the data over all days
d_long_exp_agg <- d_long %>%
    filter(condition == "Experience")
x <- d_long_exp_agg$loneliness[d_long_exp_agg$timepoint == "before"]
y <- d_long_exp_agg$loneliness[d_long_exp_agg$timepoint == "after"]
ttest_agg <- t.test(x, y, paired = TRUE); print(ttest_agg)
cd <- cohen.d(x, y, paired = TRUE); print(cd)

# Print means before and after
print(paste0("Mean before: ", mean(x)))
print(paste0("Mean after: ", mean(y)))

# Table S3: PAIRED T-TESTS COMPARING LONELINESS BEFORE VS. AFTER INTERACTION
print("day,mbefore,sdbefore,mafter,sdafter,t,p,d")
for (i in 1:7) {
    print(paste0("------- Day ", i, " -------"))
    d_long_exp_day <- d_long %>%
        filter(day == i & condition == "Experience")
    x <- d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]
    y <- d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]
    ttest <- t.test(x, y, paired = TRUE)
    cd <- cohen.d(x, y, paired = TRUE)
    
    print(paste0(i, "; MBefore = ", round(mean(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]), 2), " (", round(sd(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]), 2), ") vs. MAfter = ", round(mean(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]), 2), " (", round(sd(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]), 2), "), t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p < .001", ", d = ", round(cd$estimate, 2)))
}

# Do an ANOVA test on only the control subset, to see if 'day' has an effect on loneliness
d_long_control <- d_long %>%
  filter(condition == "Control")

d_long_control$day <- as.numeric(d_long_control$day)
model_control <- lmer(loneliness ~ day + (1|worker_id), data = d_long_control)
summary(model_control)

## Remove day 1 and re-run the model
d_long_exp_no1 <- d_long_exp[d_long_exp$day != 1,]
model_no1 <- lmer(loneliness ~ day * timepoint + (1|worker_id), data = d_long_exp_no1)
summary(model_no1)


### Second, in order to determine whether loneliness levels after experiencing the chatbot were lower than in the control condition, 
### we ran the following ANOVA model on data from both the control condition and the ‘after’ measurements from the experience condition: 
### “Loneliness ~ Condition * Day + (1 | Participant ID)”.
d_long_control_expafter <- d_long %>%
  filter(condition == "Control" | (condition == "Experience" & timepoint == "after"))
d_long_control_expafter$day <- as.numeric(as.factor(d_long_control_expafter$day))
model_control <- lmer(loneliness ~ condition * day + (1|worker_id), data = d_long_control_expafter)
summary(model_control)

# Table S4: T-TESTS COMPARING LONELINESS IN CONTROL VS. AFTER INTERACTION
for (i in 1:7) {
    d_long_control_exp_day <- d_long %>%
        filter(day == i & (condition == "Control" | (condition == "Experience" & timepoint == "after")))
    x <- d_long_control_exp_day$loneliness[d_long_control_exp_day$condition == "Control"]
    y <- d_long_control_exp_day$loneliness[d_long_control_exp_day$timepoint == "after" & d_long_control_exp_day$condition == "Experience"]
    ttest_control_exp <- t.test(x, y, paired = FALSE)
    cd <- cohen.d(x, y, paired = FALSE)

    # Print the results in this format: MControl = 4.37 (2.13) vs. MAfter = 5.91 (3.11), t(3177.7) = -2.96, p = .003, d = -0.10
    print(paste0(i, "; MControl = ", round(mean(x), 2), " (", round(sd(x), 2), ") vs. MAfter = ", round(mean(y), 2), " (", round(sd(y), 2), "), t(", round(ttest_control_exp$parameter, 2), ") = ", round(ttest_control_exp$statistic, 2), ", p = ", round(ttest_control_exp$p.value, 3), ", d = ", round(cd$estimate, 2)))
}


### Third, in order to assess whether there was a difference in predicted versus actual drops in loneliness, we ran another ANOVA model, with the loneliness difference between before and after ratings on both prediction and experience conditions as the DV, 
### and condition and day as IV’s, i.e., we used the following model: 
### “Loneliness Difference ~ Condition * Day + (1 | Participant ID)”
d_long_diff_exp <- d_long_diff
d_long_diff_exp$day <- as.numeric(as.factor(d_long_diff_exp$day))

model_diff <- lmer(diff ~ condition * day + (1|worker_id), data = d_long_diff_exp)
summary(model_diff)

# Remove day 1 and re-run the model
d_long_diff_exp_no1 <- d_long_diff_exp[d_long_diff_exp$day != 1,]
model_diff_no1 <- lmer(diff ~ condition * day + (1|worker_id), data = d_long_diff_exp_no1)
summary(model_diff_no1)

# Additionally, for each day, there was no significant difference in loneliness between the prediction and experience conditions 

# Run t-tests comparing the difference in loneliness before vs. after interaction between prediction and interaction conditions
for (i in 1:7) {
    print(paste0("------- Day ", i, " -------"))
    d_long_diff_pred_day <- d_long_diff %>%
        filter(day == i)
    x <- d_long_diff_pred_day$diff[d_long_diff_pred_day$condition == "Prediction"]
    y <- d_long_diff_pred_day$diff[d_long_diff_pred_day$condition == "Experience"]
    ttest_diff <- t.test(x, y, paired = FALSE)
    print(ttest_diff)

    cd <- cohen.d(x, y, paired = FALSE)
    print(cd)
}

# When we aggregated the data over all 7 days, we found that participants significantly underestimated the chatbot’s ability to reduce loneliness
d_long_diff_pred <- d_long_diff %>%
    filter(condition == "Prediction" | condition == "Experience")

x <- d_long_diff_pred$diff[d_long_diff_pred$condition == "Prediction"]
y <- d_long_diff_pred$diff[d_long_diff_pred$condition == "Experience"]

ttest_diff <- t.test(x, y, paired = FALSE); print(ttest_diff)
cd <- cohen.d(x, y, paired = FALSE); print(cd)


# Run t-tests comparing loneliness in the control condition vs. loneliness after interaction in the experience condition
d_long_control_exp <- d_long %>%
    filter(condition == "Control" | (condition == "Experience" & timepoint == "after"))

x <- d_long_control_exp$loneliness[d_long_control_exp$condition == "Control"]
y <- d_long_control_exp$loneliness[d_long_control_exp$timepoint == "after" & d_long_control_exp$condition == "Experience"]
ttest_control_exp <- t.test(x, y, paired = FALSE); print(ttest_control_exp)
cd <- cohen.d(x, y, paired = FALSE); print(cd)


# Run t-test comparing loneliness in the control condition vs. loneliness before interaction in the experience condition
d_long_control_exp_before <- d_long %>%
    filter(condition == "Control" | (condition == "Experience" & timepoint == "before"))

x <- d_long_control_exp_before$loneliness[d_long_control_exp_before$condition == "Control"]
y <- d_long_control_exp_before$loneliness[d_long_control_exp_before$timepoint == "before" & d_long_control_exp_before$condition == "Experience"]
ttest_control_exp_before <- t.test(x, y, paired = FALSE); print(ttest_control_exp_before)
cd <- cohen.d(x, y, paired = FALSE); print(cd)


################################### ANALYZE LONELY USERS ################################

d_long_exp_control <- d_long %>%
    filter(condition == "Experience" | condition == "Control")

# Filter users in the experience condition in d_long, who have higher before-interaction loneliness than the mean of the before-interaction loneliness in the experience condition
d_long_before <- d_long_exp_control %>%
    filter(timepoint == "before" & day == 1)

threshold <- mean(d_long_before$loneliness)

# Only select users who have mean_before higher than the threshold
high_before_loneliness_users <- d_long_before[d_long_before$loneliness > threshold, 'worker_id']
low_before_loneliness_users <- d_long_before[d_long_before$loneliness <= threshold, 'worker_id']

# In d_long, only keep users who are in the high_loneliness_users list for the experience condition, i.e., remove low_loneliness_users
d_long_high_loneliness <- d_long_exp_control[!(d_long_exp_control$worker_id %in% low_before_loneliness_users$worker_id),]
d_long_low_loneliness <- d_long_exp_control[!(d_long_exp_control$worker_id %in% high_before_loneliness_users$worker_id),]

table(d_long_high_loneliness$condition, d_long_high_loneliness$day, d_long_high_loneliness$timepoint)
table(d_long_low_loneliness$condition, d_long_low_loneliness$day, d_long_low_loneliness$timepoint)


# TABLE S6: T-TESTS COMPARING LONELINESS IN CONTROL VS. AFTER INTERACTION WITHIN THE SUBSET OF LONELY USERS
for (i in 1:7) {
    d_long_control_exp_day <- d_long_high_loneliness %>%
        #filter(day == i & (condition == "Experience"))
        filter(day == i & (condition == "Control" | (condition == "Experience" & timepoint == "after")))
    x <- d_long_control_exp_day$loneliness[d_long_control_exp_day$condition == "Control"]
# x <- d_long_control_exp_day$loneliness[d_long_control_exp_day$timepoint == "before" & d_long_control_exp_day$condition == "Experience"]
    y <- d_long_control_exp_day$loneliness[d_long_control_exp_day$timepoint == "after" & d_long_control_exp_day$condition == "Experience"]
    ttest_control_exp <- t.test(x, y, paired = FALSE)
    cd <- cohen.d(x, y, paired = FALSE)
    # Print the results in this format: MControl = 4.37 (2.13) vs. MAfter = 5.91 (3.11), t(3177.7) = -2.96, p = .003, d = -0.10
    print(paste0(i, "; MControl = ", round(mean(x), 2), " (", round(sd(x), 2), ") vs. MAfter = ", round(mean(y), 2), " (", round(sd(y), 2), "), t(", round(ttest_control_exp$parameter, 2), ") = ", round(ttest_control_exp$statistic, 2), ", p = ", round(ttest_control_exp$p.value, 3), ", d = ", round(cd$estimate, 2)))

}


################################ BOOTSTRAPPING WITH REPLACEMENT ################################

set.seed(123)  # For reproducibility

# TABLE S5: T-TESTS COMPARING LONELINESS IN CONTROL VS. AFTER INTERACTION, AFTER BOOTSTRAPPING
for (i in 1:7) {
    print(paste0("------- Day ", i, " -------"))
    d_long_control_exp_day <- d_long %>%
        filter(day == i & (condition == "Control" | (condition == "Experience" & timepoint == "after")))
    x <- d_long_control_exp_day$loneliness[d_long_control_exp_day$condition == "Control"]
    y <- d_long_control_exp_day$loneliness[d_long_control_exp_day$timepoint == "after" & d_long_control_exp_day$condition == "Experience"]
    
    # Combine x and y into a data frame
    data_combined <- data.frame(
        loneliness = c(x, y),
        group = rep(c("Control", "Experience"), times = c(length(x), length(y)))
    )
    
    # Double the sample size by bootstrapping
    n_bootstrap <- 2000  # Number of bootstrap samples
    t_values <- numeric(n_bootstrap)
    p_values <- numeric(n_bootstrap)
    cohen_d_values <- numeric(n_bootstrap)
    control_values <- numeric(n_bootstrap)
    experience_values <- numeric(n_bootstrap)
    dof_values <- numeric(n_bootstrap)
    
    for (b in 1:n_bootstrap) {
        # Resample with replacement
        x_bootstrap <- sample(x, size = 4 * length(x), replace = TRUE)
        y_bootstrap <- sample(y, size = 4 * length(y), replace = TRUE)
        
        # Combine into a data frame
        data_bootstrap <- data.frame(
            loneliness = c(x_bootstrap, y_bootstrap),
            group = rep(c("Control", "Experience"), times = c(length(x_bootstrap), length(y_bootstrap)))
        )
        
        # Perform t-test
        control <- data_bootstrap[data_bootstrap$group == "Control", "loneliness"]
        experience <- data_bootstrap[data_bootstrap$group == "Experience", "loneliness"]

        ttest <- t.test(control, experience, data = data_bootstrap)
        t_values[b] <- ttest$statistic
        p_values[b] <- ttest$p.value
        control_values[b] <- mean(control)
        experience_values[b] <- mean(experience)
        dof_values[b] <- ttest$parameter
        
        # Calculate Cohen's d
        cd <- cohen.d(x_bootstrap, y_bootstrap, paired = FALSE)
        cohen_d_values[b] <- cd$estimate
    }
    
    # Calculate the mean t-statistic, mean p-value, mean effect size, and mean degrees of freedom
    mean_t <- mean(t_values)
    mean_p <- mean(p_values)
    mean_d <- mean(cohen_d_values)
    mean_control <- round(mean(control_values), 2)
    sd_control <- round(sd(control_values), 2)
    mean_experience <- round(mean(experience_values), 2)
    sd_experience <- round(sd(experience_values), 2)
    mean_dof <- round(mean(dof_values), 2)
    
    # Print the bootstrapped results
    print(paste0(i, " (Bootstrapped); MControl = ", mean_control, " (", sd_control, ")", ", MExp = ", mean_experience, " (", sd_experience, "), ", " Mean t = ", round(mean_t, 2), ", Mean p = ", round(mean_p, 3), ", Mean d = ", round(mean_d, 2), ", Mean dof = ", round(mean_dof, 2)))
}

################################### BOOTSTRAPPING WITHOUT REPLACEMENT ################################

for (i in 1:7) {
    print(paste0("------- Day ", i, " -------"))
    d_long_control_exp_day <- d_long %>%
        filter(day == i & (condition == "Control" | (condition == "Experience" & timepoint == "after")))
    x <- d_long_control_exp_day$loneliness[d_long_control_exp_day$condition == "Control"]
    y <- d_long_control_exp_day$loneliness[d_long_control_exp_day$timepoint == "after" & d_long_control_exp_day$condition == "Experience"]
    
    # Double the sample size by sampling with replacement
    x_bootstrap <- c(x, x) #sample(x, size = 2 * length(x), replace = FALSE)
    y_bootstrap <- c(y, y) #sample(y, size = 2 * length(y), replace = FALSE)

    # Perform t-test on bootstrapped samples
    ttest_control_exp <- t.test(x_bootstrap, y_bootstrap, paired = FALSE)
    cd <- cohen.d(x_bootstrap, y_bootstrap, paired = FALSE)
    
    # Print the results
    print(paste0(i, " (Bootstrapped); MControl = ", round(mean(x_bootstrap), 2), " (", round(sd(x_bootstrap), 2), ") vs. MAfter = ", round(mean(y_bootstrap), 2), " (", round(sd(y_bootstrap), 2), "), t(", round(ttest_control_exp$parameter, 2), ") = ", round(ttest_control_exp$statistic, 2), ", p = ", round(ttest_control_exp$p.value, 3), ", d = ", round(cd$estimate, 2)))
}

######################## EXPLORATORY ANALYSES ##########################

d_long_diff_exp <- d_long_diff %>%
    filter(condition == "Experience")

# Run mixed effects regression to see if n_messages and avg_n_words predict loneliness difference
summary(lmer(diff ~ n_messages + avg_n_words + (1|worker_id), data = d_long_diff_exp))

# Plot diff and n_messages together
ggplot(data = d_long_diff_exp, aes(x = n_messages, y = diff)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    theme_classic() +
    labs(title = "Difference in Loneliness Scores vs. Number of Messages",
         x = "Number of Messages",
         y = "Difference in Loneliness Score") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 0, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "top", legend.direction = "horizontal")


d_long_exp_after <- d_long_engagement %>%
    filter(condition == "Experience" & timepoint == "after")

# Run mixed effects regression to see if n_messages and avg_n_words predict loneliness
summary(lmer(loneliness ~ n_messages + avg_n_words + (1|worker_id), data = d_long_exp_after))

######################## EFFECT SIZES ########################

# Now, calculate the effect sizes for different loneliness levels
# For this, divide the data into two groups: lonely and not lonely. 
# To do this, we have several constant loneliness thresholds ranging from 20 to 80.
# Then, we will calculate the effect size for each group
loneliness_thresholds <- (0:100)


d_long_exp <- d_long %>%
  filter(condition == "Experience")

# Initialize effect_sizes as an empty list
effect_sizes <- list()
loneliness_diffs <- list()

before <- d_long_exp[d_long_exp$timepoint == "before", 'loneliness']
after <- d_long_exp[d_long_exp$timepoint == "after", 'loneliness']

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

################ LESS ENGAGED PARTICIPANTS ################

# Find mean number of messages
mean_msgs <- mean(d_long_engagement$n_messages, na.rm = TRUE)
print(paste0("Mean number of messages: ", mean_msgs))

table(d_long_engagement$condition, d_long_engagement$timepoint, d_long_engagement$day)

# Replace NA n_messages with 0
d_long_engagement$n_messages[is.na(d_long_engagement$n_messages)] <- 0

# Exclude participants who sent less than mean number of messages across all days
# For this, first calculate mean number of messages sent by each participant
d_long_engagement$mean_n_messages <- ave(d_long_engagement$n_messages, d_long_engagement$worker_id, FUN = mean)

# Get participants with mean number of messages less than the overall mean
participants_high_n_messages <- na.omit(unique(d_long_engagement[d_long_engagement$condition == "Experience" & d_long_engagement$mean_n_messages > mean_msgs, 'worker_id'])) # 133
participants_low_n_messages <- na.omit(unique(d_long_engagement[d_long_engagement$condition == "Experience" & d_long_engagement$mean_n_messages <= mean_msgs, 'worker_id']))

d_long_engagement_low_n_messages <- d_long_engagement[!(d_long_engagement$worker_id %in% participants_high_n_messages),]
d_long_engagement_high_n_messages <- d_long_engagement[!(d_long_engagement$worker_id %in% participants_low_n_messages),]

# TABLE S7: T-TESTS COMPARING LONELINESS BEFORE VS. AFTER INTERACTION WITHIN THE SUBSET OF LESS-ENGAGED PARTICIPANTS
print("day,mbefore,sdbefore,mafter,sdafter,t,p,d")
for (i in 1:7) {
    print(paste0("------- Day ", i, " -------"))
    d_long_exp_day <- d_long_engagement_low_n_messages %>%
        filter(day == i & condition == "Experience")
    x <- d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]
    y <- d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]
    ttest <- t.test(x, y, paired = TRUE)
    cd <- cohen.d(x, y, paired = TRUE)
    
    print(paste0(i, "; MBefore = ", round(mean(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]), 2), " (", round(sd(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "before"]), 2), ") vs. MAfter = ", round(mean(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]), 2), " (", round(sd(d_long_exp_day$loneliness[d_long_exp_day$timepoint == "after"]), 2), "), t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p < .001", ", d = ", round(cd$estimate, 2)))
}

###################### PROPENSITY SCORE MATCHING ######################

if(FALSE) { # Make TRUE if you want to run this

  # First, we will run a propensity score matching analysis to match participants in the control and experience conditions based on
  # Gender, Relationship status, Age, AI experience, and Household Income
  demographics <- read.csv("data/demographics_day7.csv")
  
  # In the demographics dataframe, only include participants that are in 'd' dataframe
  demographics <- demographics[demographics$ParticipantId %in% d$worker_id,]
  
  # Now add 'Condition' column to the demographics dataframe, by reading the condition from the main dataframe, by finding the condition based on the worker_id
  # Iterate over each row in the demographics dataframe, and find the condition based on the worker_id
  demographics$condition <- NA
  demographics$ai_experience <- NA
  demographics$ai_capability <- NA
  for (i in 1:dim(demographics)[1]) {
    worker_id <- demographics$ParticipantId[i]
    condition <- d[d$worker_id == worker_id, 'condition'][1]
    demographics$condition[i] <- condition
    demographics$ai_experience[i] <- d[d$worker_id == worker_id, 'ai_companion_exp'][1]
    demographics$ai_capability[i] <- as.numeric(d[d$worker_id == worker_id, 'ai_capability_1'][1])
    
    # Also add the loneliness scores before interaction
    demographics$loneliness_before[i] <- d[d$worker_id == worker_id, 'exp_lonely_before_1'][1]
    
    # Also add the loneliness scores in the control condition
    demographics$loneliness_control[i] <- d[d$worker_id == worker_id, 'control_lonely'][1]
  }
  
  
  ########## SIMPLIFYING DEMOGRAPHICS ########
  
  # In marital status, replace 'Divorced', 'Separated', 'Widowed' with single
  demographics$`Relationship.Marital.Status` <- ifelse(demographics$`Relationship.Marital.Status` %in% c("Divorced", "Separated", "Widowed"), "Single", demographics$`Relationship.Marital.Status`)
  
  # Also replace 'Married', 'In a civil union/partnership' as 'In a relationship'
  demographics$`Relationship.Marital.Status` <- ifelse(demographics$`Relationship.Marital.Status` %in% c("Married", "In a civil union/partnership"), "In a relationship", demographics$`Relationship.Marital.Status`)
  
  # Also simplify 'Household.Income' column
  demographics <- demographics %>%
    mutate(Broad.Income.Category = case_when(
      `Household.Income` %in% c("Less than $10,000", "$10,000-$19,999", "$20,000-$29,999") ~ "<$10,000 - $29,999",
      `Household.Income` %in% c("$30,000-$39,999", "$40,000-$49,999", "$50,000-$59,999") ~ "$30,000 - $59,999",
      `Household.Income` %in% c("$60,000-$69,999", "$70,000-$79,999", "$80,000-$89,999", "$90,000-$99,999") ~ "$60,000 - $99,999",
      `Household.Income` %in% c("$100,000-$124,999", "$125,000-$149,999", "$150,000-$174,999", "$150,0000-$174,999") ~ "$100,000 - $174,999",
      `Household.Income` %in% c("$175,000-$199,999", "$200,000-$224,999", "$225,000-$249,999", "$250,000 or more") ~ "$175,000 or more",
      `Household.Income` == "Prefer not to say" ~ "Unknown",
      TRUE ~ as.character(`Household.Income`)
    ))
  
  # Simplify Employment.Status into Employed, Unemployed, Student, and Unknown
  demographics$Employment.Status <- ifelse(demographics$Employment.Status %in% c("Full-time", "Part-time", "Business Owner"), 
                                           "Employed", demographics$Employment.Status)
  
  # Replace 'Not in paid work (e.g., homemaker, disabled)', 'Retired' with 'Unemployed'
  demographics$Employment.Status <- ifelse(demographics$Employment.Status %in% c("Not in paid work (e.g., homemaker, disabled)", "Retired"), "Unemployed", demographics$Employment.Status)
  
  
  # Replace Experience with 1 and Control with 0
  demographics$condition <- ifelse(demographics$condition == "Experience", 1, 0)
  
  # See if any of these variables affect loneliness
  # First, convert all categorical variables to factors
  demographics$Gender <- as.factor(demographics$Gender)
  demographics$`Relationship.Marital.Status` <- as.factor(demographics$`Relationship.Marital.Status`)
  demographics$`Broad.Income.Category` <- as.factor(demographics$`Broad.Income.Category`)
  demographics$ai_experience <- as.factor(demographics$ai_experience)
  demographics$ai_capability <- as.numeric(demographics$ai_capability)
  demographics$Education <- as.factor(demographics$Education)
  demographics$Employment.Status <- as.factor(demographics$Employment.Status)
  
  # Now, do the regression
  res <- lm(condition ~ Age + Gender + ai_experience + ai_capability + `Broad.Income.Category` + `Relationship.Marital.Status` + `Race` + `Employment.Status` + Education, data = demographics)
  summary(res)
  
  # Check and compare all these variables between the two conditions
  for (col in c("Age", "Gender", "ai_experience", "Broad.Income.Category", "Relationship.Marital.Status", "Race", "Employment.Status", "Education")) {
    print(paste0("------- ", col, " -------"))
    
    # Ensure the data column is not a factor when expected to be numeric
    if (col %in% c("Age", "ai_capability")) {
      if (is.numeric(demographics[[col]])) {
        t_test_result <- t.test(demographics[demographics$condition == 0, col], demographics[demographics$condition == 1, col])
        print(t_test_result)
      } else {
        print(paste0("Warning: ", col, " expected to be numeric but was found to be ", class(demographics[[col]])))
      }
    } else {
      # Continuing with categorical variables
      levels_set <- levels(as.factor(demographics[[col]]))
      for (level in levels_set) {
        print(paste0("------- ", col, ": ", level, " -------"))
        
        # Creating counts
        success_condition_0 <- sum(demographics[demographics$condition == 0, col] == level, na.rm = TRUE)
        success_condition_1 <- sum(demographics[demographics$condition == 1, col] == level, na.rm = TRUE)
        total_condition_0 <- sum(demographics$condition == 0, na.rm = TRUE)
        total_condition_1 <- sum(demographics$condition == 1, na.rm = TRUE)
        
        # Constructing matrix
        matrix_data <- matrix(c(success_condition_0, success_condition_1,
                                total_condition_0 - success_condition_0, 
                                total_condition_1 - success_condition_1),
                              nrow = 2, byrow = TRUE,
                              dimnames = list(c("Condition 0", "Condition 1"), 
                                              c("Success", "Failure")))
        
        # Condition to choose proper test based on sample size and cell counts
        if(min(matrix_data) < 5) {
          # Use exact test if any cell count is less than 5
          test_result <- exact2x2(matrix_data)
          print(test_result)
        } else {
          # Use prop.test otherwise
          prop_test_result <- prop.test(matrix_data)
          print(prop_test_result)
        }
      }
    }
  }
  
  ###################### PSM nearest neighbor ######################
  
  m.out <- matchit(condition ~ Age + Gender + ai_experience + `Broad.Income.Category` + `Education` + `Relationship.Marital.Status` + Race + Employment.Status, 
                   data = demographics, method = "nearest", ratio = 1, caliper = 0.05)
  summary_output <- summary(m.out)
  summary_output
  
  # Convert each component to a data frame and write to a CSV file
  for(i in 1:length(summary_output)){
    if(is.list(summary_output[[i]]) || is.matrix(summary_output[[i]])){
      component_df <- as.data.frame(summary_output[[i]])
      
      # Round all numeric values to two decimal places
      component_df <- round(component_df, 2)
      
      write.csv(component_df, file = paste0("summary_output_component_", i, ".csv"))
    }
  }
  
  # Mean differences: 
  # nearest glm: mean diff: 0.38, eCDF Mean = 0.104, eCDF Max = 0.214, str. pair dist = 0.38
  # nearest logit: mean diff: 0.38, eCDF Mean = 0.104, eCDF Max = 0.214
  # subclass: mean diff: 0.08, eCDF mean = 0.016, eCDF max = 0.070
  
  par(mfrow = c(3, 3))  # Adjust the numbers based on the number of plots
  for(plot_type in c("Age", "Gender", "ai_experience", "Broad.Income.Category", "Relationship.Marital.Status", "Education", "Race", "Employment.Status")) {
    plot(m.out, type = "density", interactive = FALSE,
         which.xs = plot_type)
  }
  
  # Show the plot
  plot(m.out, type = "jitter")
  plot(m.out, type = "hist")
  
  
  # Get two groups: matched and unmatched
  matched_data <- match.data(m.out)
  
  d_orig <- d
  
  ##### IMPORANT: Filter out unmatched participants in d dataframe but also include the prediction condition #####
  d <- d[d$worker_id %in% matched_data$ParticipantId | (d$condition == "Prediction"),]
  
  # Now the dataframe is matched, we can run the same analyses as before
  
  # Get how many participants are in each condition
  print(table(d$condition, d$day))
    
}

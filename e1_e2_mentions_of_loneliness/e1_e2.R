rm(list = ls())

if (!require(pacman)) { install.packages(pacman) }
pacman::p_load('effsize')
pacman::p_load('ltm')
pacman::p_load('rjson')
pacman::p_load('stringr')

pacman::p_load("RSQLite")
pacman::p_load("dplyr")
pacman::p_load("tidyr")
pacman::p_load("ggplot2")
pacman::p_load("devtools")
pacman::p_load("lsr")
pacman::p_load("ggpubr")
pacman::p_load_gh("skranz/dplyrExtras")

# Manually enter directory path if you are not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Note. ‘.’= p < .1, ‘**’ = p < .01, ‘***’ = p < .001.
print_sig <- function(p) {
  if (p < 0.001) {
    print("Significance: ***")
  } else if (p < 0.01) {
    print("Significance: **")
  } else if (p < 0.05) {
    print("Significance: *")
  } else if (p < 0.1) {
    print("Significance: .")
  } else {
    print("not significant")
  }
}

# A function that reads a date and calculates how many days have passed since that date
days_since <- function(date) {
  date <- as.Date(date, format = "%Y-%m-%d")
  today <- as.Date("2024-01-24", format = "%Y-%m-%d")
  return(as.numeric(today - date))
}


# Function to read a given text and search for the given term
find_words <- function(needles, haystack) {
  # Lower all the characters in haystack and needles
  needles <- tolower(needles)
  haystack <- tolower(haystack)
  
  # Initialize a vector to store the results
  results <- logical(length(needles))
  
  # Check each needle in the haystack
  for (i in seq_along(needles)) {
    results[i] <- grepl(needles[i], haystack, fixed = TRUE)
  }
  
  # Remove NA values from the results
  results <- results[!is.na(results)]
  return(toJSON(needles[results]))
}

############################### For conversation data (Study 1A) ###############################

# All conversations that are manually verified as lonely by both raters
all_data <- read.csv('./e1_data/cleverbot_results_anonymized.csv')

spl <- function(s) {return(str_split(s, '_')[[1]][1])}
print(paste0("Number of users: ", length(unique(lapply(all_data[,'id'], spl)))))
print(paste0("Number of conversations: ", dim(all_data)[1]))
print(paste0("Conversations per user: ", dim(all_data)[1] / length(unique(lapply(all_data[,'id'], spl)))))
print(paste0("% of conversations containing loneliness detected by loneliness dictionary (N=90): ", round(100 * sum(all_data$contains_loneliness_dict == "True") / dim(all_data)[1], 2)))

# All message pairs that are classified as lonely by the LLM. This file also includes manual classification data
message_pairs <- read.csv('./e1_data/message_pairs_manual.csv', sep=";")

# both raters agreed (α = 0.84) 
cronbach.alpha(message_pairs[, c('rater_1', 'rater_2')])
agreements <- message_pairs[message_pairs$rater_1 == message_pairs$rater_2, ]

print(paste0("% of lonely message pairs (N=221): ", round(100 * sum(agreements$rater_1 == 1)) / dim(message_pairs)[1], 2))
print(paste0("% of conversations truly containing loneliness (N=156): ", round(100 * sum(all_data$contains_loneliness_llm == 1) / dim(all_data)[1], 2)))


# *-*-*-*-*-*-* # Engagement *-*-*-*-*-*-* #

# Tests for loneliness v. non-loneliness comparisons
# Use wilcox test if distribution is not normal, else use regular t-tests
for (dv in c('duration_in_mins', 'turns', 'human_word_amt')) {
    print(paste("*-*-*-*-*-*", dv, "*-*-*-*-*-*"))

    lonely <- all_data[all_data$contains_loneliness_llm == 1, dv]
    non_lonely <- all_data[all_data$contains_loneliness_llm == 0, dv]
    
    print("---- Means ----")
    #print(paste("Mean of all: ", mean(all_data[, dv])))
    print(paste("Mean of loneliness: ", mean(lonely)))
    print(paste("Mean of non-loneliness: ", mean(non_lonely)))
    
    print("---- Medians ----")
    #print(paste("Median of all: ", median(all_data[, dv])))
    print(paste("Median of loneliness: ", median(lonely)))
    print(paste("Median of non-loneliness: ", median(non_lonely)))

    print("----------- Loneliness v. Non-loneliness -----------")
    wtest <- wilcox.test(x = lonely, y = non_lonely, na.rm = TRUE,
                          paired = FALSE, exact = FALSE, conf.int = TRUE)
    print(wtest)

    # Calculate Z-value:
    print(paste0("Z value: ", qnorm(wtest$p.value / 2)))
    print(cohen.d(lonely, non_lonely))
}

############################### For review data (Study 1B) ###############################

# Create an empty dataframe that contains 'sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app' columns
d_review_all <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(d_review_all) <- c('sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app')

for (app in c('replika', 'chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt')) {   #'cleverbot',
  # Read './review_data/{app}/{app}_reviews.csv'

  print(paste0("*-*-*-*-*-* ", app, " *-*-*-*-*-*"))
  app_review <- read.csv(paste0('./e2_data/review_data/', app, '/results/results_with_inference.csv')) # Change review_data to conversation_data if you are using conversation data

  # Read sentiments in 'sentiment' column as a JSON object. Get the first item in the object
  # and get the 'sentiment' value
  sent <- lapply(app_review$sentiment, fromJSON)
  sent <- lapply(sent, function(x) {return(x[[1]])})
  
  sent <- unlist(sent)
  app_review$sentiment <- sent

  loneliness <- app_review[app_review$contains_loneliness_llm == 1, ]
  non_loneliness <- app_review[app_review$contains_loneliness_llm == 0, ]

  # Save the app into d_review_all
  d_review_all <- rbind(d_review_all, app_review[, c('sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app')])

  # Print % of reviews containing 'feeling heard'
  app_review$feeling_heard <- sapply(app_review$review, find_words, needles = c('feeling heard', 'feel heard', 'felt heard'))

  print(paste0("Number of reviews: ", dim(app_review)[1]))

  print(paste0("Loneliness percentage: ", (100 * dim(loneliness)[1] / dim(app_review)[1])))
  print(paste0("Mean overall rating: ", mean(app_review[, 'rating'])))

  print(paste0("Mean rating of non-lonely users: ", mean(na.omit(non_loneliness[,'rating']))))
  print(paste0("Mean rating of lonely users: ", mean(na.omit(loneliness[,'rating']))))

  # Percentage of positive reviews in loneliness and non-loneliness
  print(paste0("Percentage of positive reviews in loneliness: ", (100 * dim(loneliness[loneliness$sentiment == "positive", ])[1] / dim(loneliness)[1])))
  print(paste0("Percentage of positive reviews in non-loneliness: ", (100 * dim(non_loneliness[non_loneliness$sentiment == "positive", ])[1] / dim(non_loneliness)[1])))

  # Average rating of lonely users
  print(paste0("MDN rating of non-lonely users: ", median(na.omit(non_loneliness[,'rating']))))
  print(paste0("MDN rating of lonely users: ", median(na.omit(loneliness[,'rating']))))

  # Mean days since the reviews are posted:
  print(paste0("Mean days since the reviews are posted: ", mean(days_since(app_review[, 'date']))))
  print(paste0("SD since the reviews are posted: ", sd(days_since(app_review[, 'date']))))

  # Oldest review
  print(paste0("Oldest review: ", min(app_review[, 'date'])))

  # Newest review
  print(paste0("Newest review: ", max(app_review[, 'date'])))

  print("----------- Loneliness v. Non-loneliness -----------")
  wtest <- wilcox.test(x = na.omit(loneliness[,'rating']), y = na.omit(non_loneliness[,'rating']), na.rm = TRUE,
                        paired = FALSE, exact = FALSE, conf.int = TRUE)

  print(wtest)

  # Calculate Z-value:
  print(paste0("Z value: ", qnorm(wtest$p.value / 2)))
  print(cohen.d(na.omit(loneliness[,'rating']), na.omit(non_loneliness[,'rating'])))

  # Compare the two percentages using a proportion test
  print("----------- Loneliness v. Non-loneliness Valence -----------")

  # Print dimension of loneliness and non-loneliness
  print(paste0("Number of loneliness reviews: ", dim(loneliness)[1]))
  print(paste0("Number of non-loneliness reviews: ", dim(non_loneliness)[1]))

  pt <- prop.test(x = c(dim(loneliness[loneliness$sentiment == "positive",])[1], dim(non_loneliness[non_loneliness$sentiment == "positive",])[1]),
            n = c(dim(loneliness)[1], dim(non_loneliness)[1]),
            alternative = "two.sided", correct = FALSE)

  print(pt)
  print("------------------ ************* ------------------")
}

############################## Plotting ##############################

d_review_all$rating <- as.numeric(d_review_all$rating)
d_review_all$contains_loneliness_llm <- as.numeric(d_review_all$contains_loneliness_llm)

# Also plot a similar plot, with app in the x-axis, and loneliness percentage in the y-axis.
# Apps should be ordered based on their average rating
d_review_all$app <- factor(d_review_all$app, levels = c('replika', 'chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt'))

d_app <- d_review_all %>%
  group_by(app) %>%
  summarise(loneliness_percentage = (100 * sum(contains_loneliness_llm) / n()),
  rating = mean(rating))

d_app_nogpt <- d_app[d_app$app != 'chatgpt', ]
cor.test(d_app_nogpt$loneliness_percentage, d_app_nogpt$rating, method = "spearman")

# Add a bar plot here, that will have 'app' in x-axis, mean rating in the y-axis, error bars, and loneliness as color fill
d_plot <- d_review_all %>%
  group_by(app, contains_loneliness_llm) %>%
  summarise(mean_rating = mean(rating),
            sd_rating = sd(rating),
            ci_rating = 1.96 * sd_rating / sqrt(n()))

# Order apps in d_plot based on their mean rating
apporder <- c('replika' , 'chatgpt', 'igirl', 'simsimi', 'chai', 'cleverbot')
d_plot$app <- factor(d_plot$app, levels = apporder)

barplot <- ggplot(d_plot, aes(x = app, y = mean_rating, fill = factor(contains_loneliness_llm))) +
  geom_bar(position="dodge", stat="identity", width = 0.9, alpha = 1, size = 0.75) +
  geom_errorbar(position=position_dodge(width=0.9), aes(ymin = mean_rating - ci_rating, ymax = mean_rating + ci_rating), width = 0.2) +
  scale_fill_manual(values = c("#cccccc", "#666666"), name = "Loneliness", guide = guide_legend(reverse = FALSE)) +
  theme_bw() +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  xlab("Apps") +
  ylab("Mean Rating")

print(barplot)

#### Feeling heard analysis

FEELING_HEARD_DICTIONARY <- read.csv('e2_data/feeling_heard_dict.csv')
FEELING_HEARD_DICTIONARY <- c(FEELING_HEARD_DICTIONARY$terms)

ratings <- read.csv('e2_data/feeling_heard_reviews.csv', sep=';')
cronbach.alpha(ratings[, c('z', 'k')])

# Get common reviews and get the percentage of reviews containing 'feeling heard'
common_ratings <- ratings[ratings$z == ratings$k, ]
table(common_ratings$z)

feeling_heard_reviews <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(feeling_heard_reviews) <- c('app', 'feeling_heard', 'review')
for (app in c('replika', 'chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt')) {
  # Read './review_data/{app}/{app}_reviews.csv'

  print(paste0("*-*-*-*-*-* ", app, " (Running, please wait...) *-*-*-*-*-*"))
  app_review <- read.csv(paste0('./e2_data/review_data/', app, '/results/results_with_inference.csv')) # Change review_data to conversation_data if you are using conversation data

  # Read sentiments in 'sentiment' column as a JSON object. Get the first item in the object
  # and get the 'sentiment' value
  sent <- lapply(app_review$sentiment, fromJSON)
  sent <- lapply(sent, function(x) {return(x[[1]])})
  
  sent <- unlist(sent)
  app_review$sentiment <- sent

  # Print % of reviews containing 'feeling heard'
  app_review$feeling_heard <- sapply(app_review$review, find_words, needles = FEELING_HEARD_DICTIONARY)
  app_review$feeling_heard_bool <- ifelse(app_review$feeling_heard == "[]", 0, 1)
  app_heard <- app_review[app_review$feeling_heard_bool == 1, c('app', 'feeling_heard', 'review')]

  # Add feeling_heard_reviews to the dataframe
  feeling_heard_reviews <- rbind(feeling_heard_reviews, app_review[app_review$feeling_heard_bool == 1, c('app', 'feeling_heard', 'review')])

  table(app_review$feeling_heard_bool)

  print(paste0("% of reviews containing 'feeling heard': ", (100 * sum(app_review$feeling_heard_bool) / dim(app_review)[1])))

  # Print percentage of reviews containing loneliness that also contain 'feeling heard'
  print(paste0("% of reviews containing loneliness that also contain 'feeling heard': ", (100 * sum(app_review$feeling_heard_bool & (app_review$contains_loneliness_llm == 1)) / sum(app_review$contains_loneliness_llm))))
}

# Randomly select 114 reviews (10% of the total reviews) and save them in a csv file
#set.seed(123)
#feeling_heard_reviews <- feeling_heard_reviews[sample(nrow(feeling_heard_reviews), 114), ]
#feeling_heard_reviews$k <- 1
#feeling_heard_reviews$z <- 1
#write.csv(feeling_heard_reviews, './review_data/feeling_heard_reviews.csv', row.names = FALSE)

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


######## REVIEWS ########

# Create an empty dataframe that contains 'sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app' columns
d_review_all <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(d_review_all) <- c('sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app')

for (app in c('replika', 'wysa', 'chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt')) {
  # Read './review_data/{app}/{app}_reviews.csv'

  print(paste0("*-*-*-*-*-* ", app, " *-*-*-*-*-*"))
  app_review <- read.csv(paste0('./review_data/review_data/', app, '/results/results_with_inference.csv')) # Change review_data to conversation_data if you are using conversation data

  sent <- lapply(app_review$sentiment, fromJSON)
  sent <- lapply(sent, function(x) {return(x[[1]])})
  
  sent <- unlist(sent)
  app_review$sentiment <- sent

  loneliness <- app_review[app_review$contains_loneliness_llm == 1, ]
  non_loneliness <- app_review[app_review$contains_loneliness_llm == 0, ]

  # Save the app into d_review_all
  d_review_all <- rbind(d_review_all, app_review[, c('sentiment', 'contains_loneliness_llm', 'rating', 'date', 'app')])

  print(paste0("Number of reviews: ", dim(app_review)[1]))

  print(paste0("Loneliness percentage: ", (100 * dim(loneliness)[1] / dim(app_review)[1])))
  print(paste0("Mean overall rating: ", mean(app_review[, 'rating'])))
  print(paste0("SD overall rating: ", sd(app_review[, 'rating'])))

  print(paste0("Mean rating of non-lonely users: ", mean(na.omit(non_loneliness[,'rating']))))
  print(paste0("SD rating of non-lonely users: ", sd(na.omit(non_loneliness[,'rating']))))

  print(paste0("Mean rating of lonely users: ", mean(na.omit(loneliness[,'rating']))))
  print(paste0("SD rating of lonely users: ", sd(na.omit(loneliness[,'rating']))))

  # Percentage of positive reviews in loneliness and non-loneliness
  print(paste0("Percentage of positive reviews in loneliness: ", (100 * dim(loneliness[loneliness$sentiment == "positive", ])[1] / dim(loneliness)[1])))
  print(paste0("Percentage of positive reviews in non-loneliness: ", (100 * dim(non_loneliness[non_loneliness$sentiment == "positive", ])[1] / dim(non_loneliness)[1])))

  # Mean percentage of overall positive reviews
  print(paste0("Mean percentage of overall positive reviews: ", (100 * dim(app_review[app_review$sentiment == "positive", ])[1] / dim(app_review)[1])))

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

# Compare Replika vs. all other apps separately (except Wysa), and Wysa vs. all other apps separately (except Replika)
for (app in c('replika', 'wysa')) {
  app_review <- d_review_all[d_review_all$app == app, ]
  
  for (other_app in c('chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt')) {
    print(paste0("-----------", app, " vs. ", other_app, " -----------"))

    other_app_review <- d_review_all[d_review_all$app == other_app, ]
    
    pt <- prop.test(x = c(dim(app_review[app_review$contains_loneliness_llm == 1,])[1], c(dim(other_app_review[other_app_review$contains_loneliness_llm == 1,])[1])),
            n = c(dim(app_review)[1], dim(other_app_review)[1]),
            alternative = "two.sided", correct = FALSE)

    print(pt)
  }
}



############################## Plotting ##############################

d_review_all$rating <- as.numeric(d_review_all$rating)
d_review_all$contains_loneliness_llm <- as.numeric(d_review_all$contains_loneliness_llm)

# Also plot a similar plot, with app in the x-axis, and loneliness percentage in the y-axis.
# Apps should be ordered based on their average rating
d_review_all$app <- factor(d_review_all$app, levels = c('replika', 'chai', 'igirl', 'simsimi', 'cleverbot', 'chatgpt', 'wysa'))

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
apporder <- c('wysa', 'replika' , 'chatgpt', 'igirl', 'simsimi', 'chai', 'cleverbot')
d_plot$app <- factor(d_plot$app, levels = apporder)

# Replace the app names with more readable names
d_plot$app <- factor(d_plot$app, labels = c('Wysa', 'Replika', 'ChatGPT', 'iGirl', 'SimSimi', 'Chai', 'Cleverbot'))

barplot <- ggplot(d_plot, aes(x = app, y = mean_rating, fill = factor(contains_loneliness_llm))) +
  geom_bar(position="dodge", stat="identity", width = 0.9, alpha = 1, size = 0.75) +
  geom_errorbar(position=position_dodge(width=0.9), aes(ymin = mean_rating - ci_rating, ymax = mean_rating + ci_rating), width = 0.2) +
  scale_fill_manual(values = c("#cccccc", "#666666"), name = "Loneliness", guide = guide_legend(reverse = FALSE)) +
  theme_bw() +
  theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
  xlab("Apps") +
  ylab("Mean Rating")

print(barplot)

# ggsave as pdf
ggsave("review_data/loneliness_rating.pdf", plot = barplot, width = 16, height = 5)
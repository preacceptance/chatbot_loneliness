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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

## ================================================================================================================
##                                                  PRE-PROCESSING
## ================================================================================================================

## read in data:
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('./data/data.csv', sep=";")
d <- d[d$Finished == 1,]
print(paste0("Number of participants hired: ", dim(d)[1]))

## perform attention exclusions:
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
print(paste0("Exclusions from attention check: ", dim(d)[1] - dim(subset(d, (d$att_1 == 2 & d$att_2 == 2)))[1]))
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))

## Perform comprehension exclusion:
print(paste0("Exclusions from comprehension check: ", dim(d)[1] - dim(subset(d, (d$comp_1 == 2 & d$comp2 == 2)))[1]))
d <- subset(d, (d$comp_1 == 2 & d$comp2 == 2))

## age
print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age

## gender
print(paste0("Percentage of females: ", dim(d[d$gender == 2,])[1] / dim(d)[1]))

# AI experience
# See d$chatbot_exp
real_exp <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0)
sum(real_exp) / length(real_exp)

## LONELINESS SCALE
loneliness <- mutate_all(d[, c('lonely_1_3', 'lonely_2_3', 'lonely_3_3', 'lonely_4_3')], function(x) as.numeric(as.character(x)))
print(paste0("Loneliness cronbach's alpha: ", cronbach.alpha(loneliness)[[1]]))

d$loneliness_scale <- rowMeans(loneliness)

## Attitude Towards AI
# Questions (positive (1) and negative (0)):
question_types <- c(1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1) # Manually took these from the qualtrics form
keys <- c()
d[, "ai_attitude_1_1"] <- d[, "ai_attitude_1_6"]
for (i in (1:16)) {
    d[, paste0("ai_attitude_", i, "_1")] <- as.numeric(d[, paste0("ai_attitude_", i, "_1")])
    keys <- append(keys, paste0("ai_attitude_", i, "_1"))
    if (question_types[i] == 0) {  # Negative question
        d[, paste0("ai_attitude_", i, "_1")] <- 100 - d[, paste0("ai_attitude_", i, "_1")]
    }
}

## calculate average items if cronbach's alpha > 0.80
cronbach.alpha(d[, keys])
d$ai_att <- rowMeans(d[, keys])

########### T-TESTS COMPARING PREDICTION VS. AFTER INTERACTION ###########
qs <- c('lonely_connect_mean','connected', 'lonely', 'entertain', 'novel', 'engage', 'comfort', 'interest')
d_merged <- data.frame(matrix(nrow = dim(d)[1] * 2, ncol = 9))
colnames(d_merged) <- append(qs, 'cond')
d_merged$cond <- c(rep(c(1), each = dim(d)[1]), rep(c(2), each = dim(d)[1])) #1 = prediction, 2 = after interaction
  
for (question in qs) {
    print(paste0("*-*-*-*-*-*-*-*-*-* ", question, " *-*-*-*-*-*-*-*-*-*"))
    if (question == 'lonely_connect_mean') {
      lonely_before <- as.numeric(d[,grepl(paste0("lonely_predict"), colnames(d)) & (colSums(d == "") == 0)])
      lonely_after <- as.numeric(d[,grepl(paste0("lonely_exp"), colnames(d)) & (colSums(d == "") == 0)])  
      
      connect_before <- as.numeric(d[,grepl(paste0("connect_predict_3"), colnames(d)) & (colSums(d == "") == 0)])
      connect_after <- as.numeric(d[,grepl(paste0("connected_exp_3"), colnames(d)) & (colSums(d == "") == 0)])
      
      before <- rowMeans(cbind(lonely_before, connect_before))
      after <- rowMeans(cbind(lonely_after, connect_after))
      
      print(paste0("Predict mean: ", mean(before), " --- After mean: ", mean(after)))
      
      vart <- var.test(before, after)
      tt <- t.test(before, after, paired = TRUE, var.equal = vart$p.value > 0.05)
      print(tt)
      print(cohen.d(before, after))
    } else if (question == 'connected') {
        d_merged[, question] <- as.numeric(c(d[, paste0("connect_predict_3")], d[, paste0("connected_exp_3")]))
        d_merged[, paste0(question, '_diff')] <- as.numeric(d[, paste0("connected_exp_3")]) - as.numeric(d[, paste0("connect_predict_3")])

        print(paste0("Predict mean: ", mean(as.numeric(d[, "connect_predict_3"])), " --- After mean: ", mean(as.numeric(d[, "connected_exp_3"]))))
        vart <- var.test(as.numeric(d[, "connect_predict_3"]), as.numeric(d[, "connected_exp_3"]))
        tt <- t.test(as.numeric(d[, "connect_predict_3"]), as.numeric(d[, "connected_exp_3"]), paired = TRUE, var.equal = vart$p.value > 0.05)
        print(tt)

        print(cohen.d(as.numeric(d[, "connect_predict_3"]), as.numeric(d[, "connected_exp_3"])))
    } else {
        d_merged[, question] <- as.numeric(c(d[, paste0(question, "_predict_3")], d[, paste0(question, "_exp_3")]))
        d_merged[, paste0(question, "_diff")] <- as.numeric(d[, paste0(question, "_predict_3")]) - as.numeric(d[, paste0(question, "_exp_3")])
        
        print(paste0("Predict mean: ", mean(as.numeric(d[, paste0(question, "_predict_3")])), " --- After mean: ", mean(as.numeric(d[, paste0(question, "_exp_3")]))))
        vart <- var.test(as.numeric(d[, paste0(question, "_predict_3")]), as.numeric(d[, paste0(question, "_exp_3")]))
        tt <- t.test(as.numeric(d[, paste0(question, "_predict_3")]), as.numeric(d[, paste0(question, "_exp_3")]), paired = TRUE, var.equal = vart$p.value > 0.05)
        print(tt)

        print(cohen.d(as.numeric(d[, paste0(question, "_predict_3")]), as.numeric(d[, paste0(question, "_exp_3")])))
    }
}

d_merged$lonely_connect_mean <- rowMeans(d_merged[, c('connected', 'lonely')])

# Calculate lonely_connect_mean_diff
d_merged$lonely_connect_mean_diff <- rowMeans(d_merged[, c('connected_diff', 'lonely_diff')])


d_merged['loneliness_scale'] <- d['loneliness_scale']

source('../process.R')

## DOES LONELINESS MODERATE EXPECTED / ACTUAL
process(data = d_merged, y = "lonely", x = "cond",
        w = c("loneliness_scale"), model = 1, seed=654321)


## ================================================================================================================
##                                              PLOTTING MAIN FIGURES
## ================================================================================================================

## plotting all measures
t_names <- c("Expected", "Actual")
title_size <- 18
axis_size <- 26

plotter <- function(y_var, title, stars) {

    ax_size <- 16  # Smaller axis size for supplemental figures
    p1 <- ggplot(d_merged, aes(x = factor(cond), y = as.numeric(y_var))) +
        theme_bw() +
        coord_cartesian(ylim = c(1, 110)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
        geom_signif(comparisons = list(c(1, 2)), textsize = 5.5, test = "t.test", test.args = c(paired = TRUE), annotation = stars)

    p1 <- p1 +
        theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_x_discrete(labels = t_names) +
        ggtitle(title) +
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(axis.text.x = element_text(size = ax_size)) +
        theme(axis.text.y = element_text(size = ax_size)) +
        theme(plot.title = element_text(size = ax_size, hjust = 0.5)) +
        geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
        stat_summary(fun.data = "mean_cl_boot", color = "black",
                     size = 0.4,
                     position = position_dodge(width = 0.9)) +
        stat_summary(fun.data = "mean_cl_boot", color = "black",
                     position = position_dodge(width = 0.9),
                     geom = "errorbar", width = 0.2)
    p1

}

# make plots
lonely_plot <- plotter(d_merged$lonely, "Less Lonely", c("**"))
connect_plot <- plotter(d_merged$connected, "More Connected", c("**"))
entertain_plot <- plotter(d_merged$entertain, str_to_title("entertained"), c("ns"))
novel_plot <- plotter(d_merged$novel, str_to_title("novel"), c("ns"))
engage_plot <- plotter(d_merged$engage, str_to_title("engage"), c("+"))
comfort_plot <- plotter(d_merged$comfort, str_to_title("comfort"), c("***"))
interest_plot <- plotter(d_merged$interest, str_to_title("interest"), c("ns"))

# main plot
dir.create(file.path('plots'))
plot_files <- list.files(pattern = c("(.pdf|.png)"))


# plot
dev.new(width = 8.5, height = 5, noRStudioGD = TRUE)
figure <- ggarrange(lonely_plot, connect_plot,
                    nrow = 1, ncol = 2, common.legend = TRUE, legend = "top")
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = axis_size, rot = 90),
                bottom = text_grob("Timing Relative to Chatbot Interaction", color = "black", face = "plain", size = axis_size, margin(b = 10), hjust = 0.50))

ggsave(
  "plot.pdf",
  last_plot(),
  dpi = 500
)


# plot
dev.new(width = 13, height = 10, noRStudioGD = TRUE)
figure <- ggarrange(entertain_plot, novel_plot, engage_plot, comfort_plot, interest_plot,
                    nrow = 2, ncol = 3, common.legend = TRUE, legend = "top")
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = axis_size, rot = 90),
                bottom = text_grob("Timing Relative to Chatbot Interaction", color = "black", face = "plain", size = axis_size, margin(b = 10), hjust = 0.50))

ggsave(
    "plot_supp.pdf",
    last_plot(),
    dpi = 500
)

dir.create(file.path('plots'))
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots", overwrite = TRUE)

######################## Does loneliness or ai attitude predict the change in qs? ######################################

d_merged_1 <- d_merged[d_merged$cond == 1,]
d_merged_1$loneliness_scale <- d$loneliness_scale
d_merged_1$ai_att <- d$ai_att

for (dv in qs) {
    print(paste("............................................", dv, "............................................"))
    for (iv in c('loneliness_scale', 'ai_att')) {
        fm <- as.formula(paste(iv, " ~ ", dv, "_diff", sep = ""))
        print(fm)
        print(summary(lm(fm, data = d_merged_1)))
    }
}

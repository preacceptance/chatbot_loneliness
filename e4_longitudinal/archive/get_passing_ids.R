# Helper script to get the worker_ids of participants who will be invited to Day 7, participants who completed Day 6 but not Day 7, and participants who completed Day 7

## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('jsonlite')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

######################################################################################################################################################

# Function for Getting IDs of participants who will be invited to Day X
get_invited_ids <- function(day) {
    # Read day X - 2 data
    d <- read.csv(paste0("data/data_day", day - 2, ".csv"))
    ids_target <- d[d$Finished == 1, 'worker_id']

    # Get participants who completed day X - 2 as well as all days before that
    for (i in 1:(day - 3)) {
        curr_day <- read.csv(paste0("data/data_day", i, ".csv"))
        curr_day_ids <- curr_day[curr_day$Finished == 1, 'worker_id']
        ids_target <- ids_target[ids_target %in% curr_day_ids]
    }

    print(paste("Number of participants who are invited to Day ", day, ":", length(ids_target)))

    # Print number of participants in each condition for ids_target
    d_all <- read.csv("data/data_day1.csv")
    d_all <- d_all[d_all$Finished == 1 & d_all$worker_id %in% ids_target,]
    print(table(d_all$condition))

    # Save the worker_ids in a txt file
    ids_target <- paste(ids_target, collapse = ",")
    write(ids_target, paste0("data/day_", day, "_target.txt"))
}

get_invited_ids(7)

######################################################################################################################################################

# Function for Getting IDs of participants who completed Day X - 1 but not Day X
get_still_incomplete_ids <- function(day) {
    # Read day X - 1 data
    d <- read.csv(paste0("data/data_day", day - 1, ".csv"))
    ids_day <- d[d$Finished == 1, 'worker_id']

    # Read day X data
    d <- read.csv(paste0("data/data_day", day, ".csv"))
    ids_day_next <- d[d$Finished == 1, 'worker_id']

    # Get participants who completed day X - 1 but not day X
    ids_still_incomplete <- ids_day[!ids_day %in% ids_day_next]

    print(paste("Number of participants who completed Day", day - 1, "but not Day", day, ":", length(ids_still_incomplete)))

    # Save the worker_ids in a txt file
    ids_still_incomplete <- paste(ids_still_incomplete, collapse = ",")
    write(ids_still_incomplete, paste0("data/day", day, "_notcompleted.txt"))
}

get_still_incomplete_ids(7)

######################################################################################################################################################

# Function for Getting IDs of participants who completed Day X
get_completed_ids <- function(day) {
    # Read day X data
    d <- read.csv(paste0("data/data_day", day, ".csv"))
    ids_completed <- d[d$Finished == 1, 'worker_id']

    # Check all days before day X, and check if they completed all days before day X
    for (i in 1:(day - 1)) {
        curr_day <- read.csv(paste0("data/data_day", i, ".csv"))
        curr_day_ids <- curr_day[curr_day$Finished == 1, 'worker_id']
        ids_completed <- ids_completed[ids_completed %in% curr_day_ids]
    }

    print(paste("Number of participants who completed Day", day, ":", length(ids_completed)))

    # Save the worker_ids in a txt file
    ids_completed_str <- paste(ids_completed, collapse = ",")
    write(ids_completed_str, paste0("data/day", day, "_completed.txt"))

    # Save the ids_completed in a csv file with Participant as the column name
    ids_completed_df <- data.frame(Participant = ids_completed)
    write.csv(ids_completed_df, paste0("data/day", day, "_completed.csv"), row.names = FALSE)

    # Bonus every participant who completed Day 7 $15
    ids_completed_bonus <- data.frame(Participant = ids_completed, Amount = 15, Message = "Thank you for your participation in our study. Your effort is greatly appreciated!")
    write.csv(ids_completed_bonus, paste0("data/day", day, "_completed_bonus.csv"), row.names = FALSE)

    # Now, get the ids of participants who are only in the experimental condition
    d_all <- read.csv("data/data_day1.csv")
    d_all <- d_all[d_all$worker_id %in% ids_completed,]
    ids_exp <- d_all[d_all$condition == "Experience", 'worker_id']
    print(paste("Number of participants who are only in the experimental condition:", length(ids_exp)))

    ids_control <- d_all[d_all$condition == "Control", 'worker_id']
    print(ids_control)
    print(paste("Number of participants who are only in the control condition:", length(ids_control)))

    # Save ids_exp in a csv file with Participant containing the ids and Amount = 0.7
    ids_exp_df <- data.frame(Participant = ids_exp, Amount = 0.7)
    write.csv(ids_exp_df, paste0("data/day", day, "_completed_exp.csv"), row.names = FALSE)
}

get_completed_ids(7)

######################################################################################################################################################

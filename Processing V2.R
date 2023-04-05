# Processing V2 

# Libraries
library(tidyverse)
library(httr)
library(jsonlite)

#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)
games_table <- read_csv("games_transformed.csv", col_names = TRUE)


### Input data (TO DO)



### TEST DATA
test_content <- readRDS("processing_test_data.RData")


test_content_timeline <- readRDS("processing_test_timeline_data.RData")

test_v5_2 <- readRDS("TestV5_2.RData")

### Stats Data
## Participant




function_0 <- function(list_to_processes, games_data){
  
  processed_data <- lapply(list_to_processes, function_1)
  
  final_df <- bind_rows(processed_data)
  
  final_df <- function_9(final_df, games_data)
  
  return(final_df)
  
}

processed_test_2 <- function_0(test_list2, games_table)

processed_test_2$team_name

view(processed_test_2)

# main funciton
function_1 <- function(raw_data){
  
  raw_data_file <- raw_data
  
  # parse json
  json_data <- fromJSON(raw_data$content)
  
  # function 2 - process structure
  participants <- function_2(json_data)
  
  # function 3 - adding relationship informaiton
  participants <- function_3(raw_data_file, json_data, participants)
  
  participants <- function_4(participants)
  
  participants <- function_5(participants)
  
  participants <- function_6(participants)
  
  participants <- function_7(participants)
  
  participants <- function_8(participants)
  
  
  return(participants)
  
}

test_1 <- function_1(test_content__2)

colnames(test_1)

view(test_1$challenges.fasterSupportQuestCompletion)
test_1$title

processed_test <- lapply(test_list2, function_1)

str(processed_test)

test_df_2 <- bind_rows(processed_test)

# process structure
function_2 <- function(data){
  
  # select participants
  # remove "perks" = runes info
  # unnest the challenges 
  participants <- data$participants
  participants <- participants %>%
    select(-perks) %>%
    unnest_wider(challenges, names_sep = ".")
  
  return(participants)
  
  
}

# add useful relationship information
function_3 <- function(raw_data_, json_data_, processing_data){
  # add game id
  processing_data$gameId <- json_data_$gameId
  
  # add title (FIX!!! SO WE DONT HAVE TO ESCAPE THE TIBBLE!!!!)
  processing_data$title <- raw_data_$stats_title
  
  return(processing_data)
  
}


#  seperate team name and player name
function_4 <- function(processing_data){
  processing_data <- processing_data %>%
    separate(summonerName, c("team_short", "summoner_name"), sep = " ", remove = FALSE)
  
  return(processing_data)
}

# replace item ids with item names
function_5 <- function(processing_data){
  processing_data[, grep("^item\\d+$", colnames(processing_data))] <- apply(processing_data[, grep("^item\\d+$", colnames(processing_data))], 2, function(col){
    unlist(item_helper[match(col, item_helper$item_id), "item_name"])
  })
  
  return(processing_data)
  
}

# replace summoner spell ids with names
function_6 <- function(processing_data){
  processing_data[, grep("^spell.*Id$", colnames(processing_data))] <- apply(processing_data[, grep("^spell.*Id$", colnames(processing_data))], 2, function(col){
    unlist(summoner_spell_helper[match(col, summoner_spell_helper$key), "name"])
  })
  
  return(processing_data)
  
}

# convert NA to 0
function_7 <- function(processing_data){
  processing_data[is.na(processing_data)] <- 0
  
  return(processing_data)
  
}


# convert logicals
function_8 <- function(processing_data){
  
  ## check for logicals
  logicals <- sapply(processing_data, is.logical)
  
  # convert logicals 
  processing_data[,logicals] <- lapply(processing_data[,logicals], as.integer)
  
  return(processing_data)
  
}


# join external info from games table
function_9 <- function(processed_tibble, games_data_){
  
  games_select <- games_data_ %>%
    select(Gamelength, Patch, StatsPage, team_name, reference_team_key, side, result)
  
  joined_df <- left_join(processed_tibble, games_select, by = c("title" = "StatsPage", "teamId" = "reference_team_key"))
  
  return(joined_df)
  
}




### Stats 
## TEAM DATA


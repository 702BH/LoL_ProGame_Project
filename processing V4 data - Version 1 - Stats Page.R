# processing V4 data - Version 1 - Stats Page
# Libraries
library(tidyverse)
library(httr)
library(jsonlite)

#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)
games_table <- read_csv("games_transformed.csv", col_names = TRUE)
champ_names_df <- read_csv("champ_id_name.csv", col_names = TRUE)


### Input data (TO DO)



### TEST DATA


test_v4_stats_1 <- readRDS("Testv4_stats_1.RData")

test_v4_stats_2 <- readRDS("Testv4_stats_2.RData")

test_v4_stats_1$stats_title
test_v4_stats_2$stats_title

### Stats Data
## Participant


function_0_v4 <- function(list_to_processes, games_data){
  
  processed_data <- lapply(list_to_processes, function_1_v4)
  
  parts_table <- lapply(processed_data, function(x) x[[1]])
  teams_table <- lapply(processed_data, function(x) x[[2]])
  
  final_parts <- bind_rows(parts_table)
  final_teams <- bind_rows(teams_table)
  
  final_parts <- function_9(final_parts, games_data)
  final_teams <- function_9(final_teams, games_data)
  
  return(list(final_parts, final_teams))
  
  
}

processed_test_v4 <- function_0_v4(test_list_v4_stats, games_table)

length(processed_test_v4)

str(processed_test_v4)

processed_parts <- processed_test_v4[[1]]

processed_teams <- processed_test_v4[[2]]


processed_parts$team_name
processed_teams$team_name
processed_parts$team_short

view(processed_teams)


function_1_v4 <- function(raw_data){
  
  raw_data_file <- raw_data
  
  # parse json
  json_data <- fromJSON(raw_data$content)
  
  # Participants
  participants <- function_2_v4(json_data)
  
  # add useful relationship information
  participants <- function_3(raw_data_file, json_data, participants)
  
  # replace item ids with item names
  participants <- function_5(participants)
  
  # replace summoner spell ids with names
  participants <- function_6(participants)
  
  # replace champion ID with names
  participants <- function_3_v4(participants)
  
  # convert NA to 0
  participants <- function_7(participants)
  
  # convert logicals
  participants <- function_8(participants)
  
  #  seperate team name and player name
  participants <- function_4_v4(json_data, participants)
  
  
  
  ## Teams data
  teams <- function_5_v4(json_data)
  
  teams <- function_8(teams)
  
  teams <- function_3(raw_data_file, json_data, teams)
  
  return(list(participants, teams))
  
}

test_v4_func <- function_1_v4(test_v4_stats_1)


# get the participants structure
function_2_v4 <- function(data){
  
  participants <- data$participants
  
  participants <- participants %>%
    select(-c(timeline, participantId)) %>%
    unnest(stats)
  
  return(participants)
  

}


# replace champion id with names
function_3_v4 <- function(data){
  
  data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
  
  return(data)
}

#  seperate team name and player name
function_4_v4 <- function(json_data, processing_data){
  
  part_identities <- json_data$participantIdentities %>%
    unnest(player) %>%
    select(-profileIcon)
  
  sep_identities <- function_4(part_identities)
  
  identities_join <-left_join(processing_data, sep_identities, by = "participantId")
  
  return(identities_join)
  
}

# teams data
function_5_v4 <- function(data){
  
  teams <- data$teams
  
  teams <- teams %>%
    unnest_wider(bans, names_sep = ".") %>%
    select(-bans.pickTurn)
  
  teams$bans.championId <- lapply(teams$bans.championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])
  
  teams$bans.championId <- sapply(teams$bans.championId, paste, collapse = ",")
  
  teams <- teams %>%
    separate(bans.championId, into = paste0("Ban", 1:5), sep = ",")
  
  return(teams)
  
  
}

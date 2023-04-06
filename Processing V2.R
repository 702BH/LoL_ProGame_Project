# Processing V2 

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
test_content <- readRDS("processing_test_data.RData")

test_V5_1 <- list(stats_title = test_content$stats_title$stats_title, content = test_content$content)


test_content_timeline <- readRDS("processing_test_timeline_data.RData")

test_v5_2 <- readRDS("TestV5_2.RData")

### Stats Data
## Participant




function_0 <- function(list_to_processes, games_data){
  
  # old method
  processed_data <- lapply(list_to_processes, function_1)
  
  parts_table <- lapply(processed_data, function(x) x[[1]])
  teams_table <- lapply(processed_data, function(x) x[[2]])
  error_titles <- lapply(processed_data, function(x) x[[3]])
  
  final_parts <- bind_rows(parts_table)
  final_teams <- bind_rows(teams_table)
  
  final_parts <- function_9(final_parts, games_data)
  final_teams <- function_9(final_teams, games_data)
  
  return(list(final_parts, final_teams, error_titles))
  
}

processed_test_2 <- function_0(test_list2, games_table)

length(processed_test_2)

str(processed_test_2)

processed_parts <- processed_test_2[[1]]

processed_teams <- processed_test_2[[2]]


processed_parts$team_name
processed_teams$team_name
processed_parts$team_short


# main funciton
function_1 <- function(raw_data){
  
  raw_data_file <- raw_data
  
  tryCatch({
    # parse json
    # old method
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
    
    
    
    ## team data
    teams <- function_10(json_data)
    
    teams <- function_8(teams)
    
    teams <- function_3(raw_data_file, json_data, teams)
    
    return(list(participants, teams, NULL))
    
  }, error = function(e){
    
    return(list(NULL, NULL, raw_data$stats_title))
  })
  
  
}

test_1 <- function_1(test_content__2)

colnames(test_1)

view(test_1$challenges.fasterSupportQuestCompletion)
test_1$title

processed_test <- lapply(test_list2, function_1)

str(processed_test)

test_df_2 <- bind_rows(processed_test)


test_combined <- function_1(test_V5_1)

parts_table <- test_combined[[1]][[1]]
teams_table <- test_combined[[2]][[2]]

view(parts_table)
view(teams_table)

teams_table$Ban1


processed_test2 <- lapply(test_list2, function_1)

teams_table_list <- lapply(processed_test2, function(x) x[[2]])

combined_table <- do.call(rbind, teams_table_list)

test <- bind_rows(teams_table_list)

view(combined_table)

view(test)
teams <- processed_test2[[2]]

view(teams)

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
  
  # old method
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
# process the team data
# structur the bans data

function_10 <- function(data){
  teams <- data$teams
  
  team_objectives <- teams$objectives
  
  unnest_obj <- do.call(data.frame, team_objectives)
  
  teams <- teams %>%
    select(-objectives)
  
  teams <- cbind(teams, unnest_obj)
  
  teams <- teams %>%
    unnest_wider(bans, names_sep = ".") %>%
    select(-bans.pickTurn)
  
  teams$bans.championId <- lapply(teams$bans.championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])
  
  teams$bans.championId <- sapply(teams$bans.championId, paste, collapse = ",")
  
  teams <- teams %>%
    separate(bans.championId, into = paste0("Ban", 1:5), sep = ",")
  
  return(teams)
  
}


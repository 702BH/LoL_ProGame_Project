# New function process
# Libraries
library(tidyverse)
library(httr)
library(jsonlite)

#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)
games_table <- read_csv("games_transformed.csv", col_names = TRUE)
champ_names_df <- read_csv("champ_id_name.csv", col_names = TRUE)



#### STATS DATA




# function_0?




function_stats_data <- function(raw_data){
  
  raw_data_file <- raw_data
  
  json_data <- fromJSON(raw_data$content)
  
  # check if contains participants and teams
  if(all(c("participants", "teams") %in% names(json_data))){
    
    #run function processing for stats
    
    ## participants data
    participants <- function_flatten_participants(json_data)
    
    # add useful relationship information
    participants <- function_add_relationships(raw_data_file, json_data, participants)
    
    #  seperate team name and player name
    participants <- function_sep_names(json_data, participants)
    
    # replace item ids with item names
    participants <- function_replace_id_values(participants, "item\\d+$", item_helper)
    
    # replace summoner spell ids with names
    participants <- function_replace_id_values(participants, "spell.*Id$", summoner_spell_helper)
    
    # replace champion ID with names
    participants <- function_replace_champs(participants)
    
    # convert NA to 0
    participants <- function_na_to_0(participants)
    
    # convert logicals
    participants <- function_logicals_to_int(participants)
    
    ## teams
    teams <- function_flatten_teams(json_data)
    
    # convert logicals
    teams <- function_logicals_to_int(teams)
    
    # add useful relationship information
    teams <- function_add_relationships(raw_data_file, json_data, teams)
    
    return(list(participants, teams))
      
  } else{
    # its a fail!
    
  }
  
  
  
}


# flatten the structure
function_flatten_participants <- function(data){
  
  participants <- data$participants
  participants <- flatten(participants)
  
  return(participants)
}




function_add_relationships <- function(raw_data_, json_data_, processing_data){
  
  # add game id
  processing_data$gameId <- json_data_$gameId
  
  # add title
  processing_data$title <- raw_data_$stats_title
  return(processing_data)
  
}




function_sep_names <- function(json_data, partiticpant_data){
  
  if(!"participantIdentities" %in% names(json_data)){
    
    partiticpant_data <- partiticpant_data %>%
      separate(summonerName, c("team_short", "summoner_name"), sep = " ", remove = FALSE)
    
    return(partiticpant_data)
    
  }else{
    
    part_identities <- json_data$participantIdentities %>%
      unnest(player) %>%
      select(-profileIcon)
    
    sep_identities <- part_identities %>%
      separate(summonerName, c("team_short", "summoner_name"), sep = " ", remove = FALSE)
    
    identities_join <-left_join(partiticpant_data, sep_identities, by = "participantId")
    
    return(identities_join)
    
    
  }
  
}



function_replace_id_values <- function(processing_data, grep_expr, key_table){
  
  processing_data[, grep(grep_expr, colnames(processing_data))] <- apply(processing_data[, grep(grep_expr, colnames(processing_data))], 2, function(col){
    unlist(key_table[match(col, key_table$key), "name"])
  })
  
  return(processing_data)
  
}


function_replace_champs <- function(data) {
  
  if("championName" %in% colnames(data)){
    return(data)
  }else{
    
    data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
    return(data)
  }
  
}

function_na_to_0 <- function(processing_data){
  processing_data[is.na(processing_data)] <- 0
  
  return(processing_data)
  
}



# convert logicals
function_logicals_to_int <- function(processing_data){
  
  ## check for logicals
  logicals <- sapply(processing_data, is.logical)
  
  # convert logicals 
  processing_data[,logicals] <- lapply(processing_data[,logicals], as.integer)
  
  return(processing_data)
  
}



function_flatten_teams <- function(data){
  
  teams <- data$teams
  
  teams <- flatten(teams)
  
  teams <- teams %>%
    unnest_wider(bans, names_sep = ".") %>%
    select(-bans.pickTurn)
  
  teams$bans.championId <- lapply(teams$bans.championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])
  
  teams$bans.championId <- sapply(teams$bans.championId, paste, collapse = ",")
  
  teams <- teams %>%
    separate(bans.championId, into = paste0("Ban", 1:5), sep = ",")
  
  return(teams)
  
  
  
}






#### TIMELINE DATA
function_timeline_data <- function(raw_data){
  
  
  raw_data_file <- raw_data
  
  json_data <- fromJSON(raw_data$content)
  
  if("frames" %in% names(json_data)){
    
    # process timeline data
    ## Participant frames
    participant_frames <- function_flatten_timeline_participants(json_data)
    
    # add game id and title
    participant_frames <- function_add_relationships(raw_data_file, json_data, participant_frames)
    
    # add team ids
    participant_frames <- function_create_team_ids(participant_frames)
    
    
    ## events
    events <- tibble(events = json_data$frames$events)
    
    events <- function_unnest_recursively_timeline(events)
    
    events <- function_add_relationships(raw_data_file, json_data, events)
    
    return(list(participant_frames, events))
    
    
  }else{
    
    # it failed
  }
  
  
}


test_timeline <- function_timeline_data(test_v5_timeline_2)


function_flatten_timeline_participants <- function(data){
  
  frames <- data$frames$timestamp
  part_frames <- data$frames$participantFrames
  
  bind_frames <- cbind(part_frames, frames)
  
  frames_pivot <- pivot_longer(bind_frames, 1:10)
  
  participant_frames_final <- frames_pivot %>%
    flatten()
  
  return(participant_frames_final)
  
}



function_create_team_ids <- function(data){
  
  part_index <- grep("participantId", names(data))
  
  participant_data <- data %>%
    mutate(teamId = case_when(
      .[[part_index]] <= 5 ~ 100,
      TRUE ~ 200
    ))
  
  return(participant_data)
  
}


function_unnest_recursively_timeline <- function(df){
  
  df_cols <- df %>%
    select_if(is.list) %>%
    names()
  
  if (length(df_cols) > 0){
    df <- df %>%
      unnest(all_of(df_cols), names_sep = ".") %>%
      function_unnest_recursively()
  }
  
  return(df)
  
}

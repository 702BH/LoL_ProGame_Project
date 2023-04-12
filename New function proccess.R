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


# combine function
function_process_combine <- function(data){
  
  # test list valid
  test_output <- function_check_input_valid_return(data)
  
  valid <- test_output[[1]]
  invalid <- test_output[[2]]
  
  # seperate valid lists
  v5_index <- unlist(lapply(valid, function(x) str_detect(x$stats_title, "V5 ")))
  timeline_index <- unlist(lapply(valid, function(x) str_detect(x$stats_title, "/Timeline")))
  
  v5_stats_list <- valid[v5_index & !timeline_index]
  v5_timeline_list <- valid[v5_index & timeline_index]
  
  v4_stats_list <- valid[!v5_index & !timeline_index]
  v4_timeline_list <- valid[!v5_index & timeline_index]
  
  
  error_titles <- c()
  
  # V5
  # stats
  if(length(v5_stats_list) > 0){
    
    v5_stats <- function_process_control(v5_stats_list, "stats")
    v5_participant_stats <- v5_stats[[1]]
    v5_team_stats <- v5_stats[[2]]
    
    error_titles <- c(error_titles, v5_stats[[3]])
    
  }else{
    v5_participant_stats <- list()
    v5_team_stats <- list()
  }
  
  
  # timeline
  if(length(v5_timeline_list) > 0){
    v5_timeline <- function_process_control(v5_timeline_list, "Timeline")
    v5_timeline_parts <- v5_timeline[[1]]
    
    v5_events_all <- v5_timeline[[2]]
    v5_events_assit_parts <- v5_timeline[[3]]
    v5_events_victim_damage_dealt <- v5_timeline[[4]]
    v5_events_victim_damage_recieved <- v5_timeline[[5]]
    
    
    
    error_titles <- c(error_titles, v5_timeline[[6]])
    
  }else{
    v5_timeline_parts <- list()
    v5_timeline_events <- list()
    
    v5_events_all <- list()
    v5_events_assit_parts <- list()
    v5_events_victim_damage_dealt <- list()
    v5_events_victim_damage_recieved <- list()
    
    
  }
  
  
  # v4
  # stats
  if(length(v4_stats_list) > 0){
    
    v4_stats <- function_process_control(v4_stats_list, "stats")
    v4_participant_stats <- v4_stats[[1]]
    v4_team_stats <- v4_stats[[2]]
    
    error_titles <- c(error_titles, v4_stats[[3]])
    
  }else{
    v4_participant_stats <-list()
    v4_team_stats <- list()
  }
  
  
  # timeline
  if(length(v4_timeline_list) > 0){
    v4_timeline <- function_process_control(v4_timeline_list, "Timeline")
    v4_timeline_parts <- v4_timeline[[1]]
    
    v4_events_all <- v4_timeline[[2]]
    v4_events_assit_parts <- v4_timeline[[3]]
    v4_events_victim_damage_dealt <- v4_timeline[[4]]
    v4_events_victim_damage_recieved <- v4_timeline[[5]]
    
    
    error_titles <- c(error_titles, v4_timeline[[6]])
    
  }else{
    v4_timeline_parts <- list()
    v4_timeline_events <- list()
    
    v4_events_all <- list()
    v4_events_assit_parts <- list()
    v4_events_victim_damage_dealt <- list()
    v4_events_victim_damage_recieved <- list()
    
  }
  
  
  return(list(v5_participant_stats = v5_participant_stats, v5_team_stats = v5_team_stats, v4_participant_stats = v4_participant_stats, 
              v4_team_stats = v4_team_stats, v5_timeline_parts = v5_timeline_parts,
              v5_events_all = v5_events_all, v5_events_assit_parts = v5_events_assit_parts,
              v5_events_victim_damage_dealt = v5_events_victim_damage_dealt, v5_events_victim_damage_recieved = v5_events_victim_damage_recieved, 
              v4_timeline_parts = v4_timeline_parts, v4_events_all = v4_events_all,
              v4_events_assit_parts = v4_events_assit_parts,
              v4_events_victim_damage_dealt = v4_events_victim_damage_dealt,
              v4_events_victim_damage_recieved = v4_events_victim_damage_recieved,
              error_matches = error_titles,
              invalid_lists = invalid))
  
  
  
  
}



# helper functions
function_check_input_valid <- function(list){
  # check current element is a list
  
  if(is.list(list) && all(c("stats_title", "content") %in% names(list))){
    content <- list[["content"]]
    if(is.character(content) && !is.na(content)){
      if(validate(content)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
  
}



function_check_input_valid_return <- function(list){
  
  check_list_results <- unlist(lapply(list, function_check_input_valid))
  
  valid_lists <- list[check_list_results]
  invalid_lists <- list[!check_list_results]
  
  return(list(valid_lists, invalid_lists))
}






#### STATS DATA




# function_0
function_process_control <- function(list_to_process, data_type){
  
  # check data type
  if(!str_detect(data_type, "Timeline")){
    
    # then its stats
    processed_data <- lapply(list_to_process, function_stats_data)
    
    parts_table <- lapply(processed_data, function(x) x[[1]])
    teams_table <- lapply(processed_data, function(x) x[[2]])
    error_titles <- lapply(processed_data, function(x) x[[3]])
    
    final_parts <- bind_rows(parts_table)
    final_teams <- bind_rows(teams_table)
    
    final_parts <- function_join_game_info(final_parts)
    final_teams <- function_join_game_info(final_teams)
    
    return(list(final_parts, final_teams, error_titles))
    
    
    
  }else{
    
    # then its timeline
    
    processed_data <- lapply(list_to_process, function_timeline_data)
    
    parts_table <- lapply(processed_data, function(x) x[[1]])
    events_table <- lapply(processed_data, function(x) x[[2]])
    error_titles <- lapply(processed_data, function(x) x[[3]])
    
    # add the seperate events tables
    events_all <- lapply(events_table, function(x) x[[1]])
    events_assit_parts <- lapply(events_table, function(x) x[[2]])
    events_victim_damage_dealt <- lapply(events_table, function(x) x[[3]])
    events_victim_damage_recieved <- lapply(events_table, function(x) x[[4]])
    
    
    final_parts <- bind_rows(parts_table)
    final_events_all <- bind_rows(events_all)
    final_events_assit_parts <- bind_rows(events_assit_parts)
    final_events_victim_damage_dealt <- bind_rows(events_victim_damage_dealt)
    final_events_victim_damage_recieved <- bind_rows(events_victim_damage_recieved)
    
    return(list(final_parts, final_events_all, final_events_assit_parts,
                final_events_victim_damage_dealt, final_events_victim_damage_recieved,
                error_titles))
    
    
  }
  
  
  
}



function_join_game_info <- function(processed_tibble){
  
  games_select <- games_table %>%
    select(Tournament, Gamelength, Patch, StatsPage, team_name, reference_team_key, side, result)
  
  joined_df <- left_join(processed_tibble, games_select, by = c("title" = "StatsPage", "teamId" = "reference_team_key"))
  
  return(joined_df)
  
}




function_stats_data <- function(raw_data){
  
  raw_data_file <- raw_data
  
  json_data <- fromJSON(raw_data$content)
  
  # check if contains participants and teams
  if(all(c("participants", "teams") %in% names(json_data))){
    
    #run function processing for stats
    
    ## participants data
    participants <- function_flatten_participants(json_data)
    
    # add useful relationship information
    participants <- function_add_relationships(participants, raw_data_file, json_data)
    
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
    teams <- function_add_relationships(teams, raw_data_file, json_data)
    
    return(list(participants, teams, NULL))
      
  } else{
    # its a fail!
    return(list(NULL, NULL, raw_data_file))
    
  }
  
  
  
}


# flatten the structure
function_flatten_participants <- function(data){
  
  participants <- data$participants
  participants <- flatten(participants)
  
  return(participants)
}




function_add_relationships <- function(processing_data, raw_data_, json_data_){
  
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
  print(raw_data$stats_title)
  
  json_data <- fromJSON(raw_data$content)
  
  if("frames" %in% names(json_data)){
    
    # process timeline data
    ## Participant frames
    participant_frames <- function_flatten_timeline_participants(json_data)
    
    # add game id and title
    participant_frames <- function_add_relationships(participant_frames, raw_data_file, json_data)
    
    # add team ids
    participant_frames <- function_create_team_ids(participant_frames)
    
    
    ## events
    events <- tibble(events = json_data$frames$events)
    
    events <- function_process_events(events)
    
    events <-   lapply(events, function_add_relationships, raw_data_file, json_data)
    
    
    
    return(list(participant_frames, events, NULL))
    
    
  }else{
    
    # it failed
    return(list(NULL, NULL, raw_data_file))
    
  }
  
  
}


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
      function_unnest_recursively_timeline()
  }
  
  return(df)
  
}


function_process_events <- function(df){
  
  temp_df <- df %>%
    unnest(events) %>%
    unnest(position, names_sep = ".") %>%
    mutate(event_id = row_number())
  
  # events df 
  events <- temp_df 
    
  
  assist_parts <- NULL
  victim_damage_dealt <- NULL
  victim_damage_recieved <- NULL
  
  # participants
  if("assistingParticipantIds" %in% names(events)){
    
    assist_parts <- events %>%
      select(event_id, assistingParticipantIds) %>%
      filter(lengths(assistingParticipantIds) > 0) %>%
      unnest(assistingParticipantIds, names_sep = ".")
    
    events <- events %>%
      select(-assistingParticipantIds)
  }
  if("victimDamageDealt" %in% names(events)){
    
    victim_damage_dealt <- events %>%
      select(event_id, victimDamageDealt) %>%
      unnest(victimDamageDealt, names_sep = ".")
    
    events <- events %>%
      select(-victimDamageDealt)
    
    
  }
  if("victimDamageReceived" %in% names(events)){
    victim_damage_recieved <- events %>%
      select(event_id, victimDamageReceived) %>%
      unnest(victimDamageReceived, names_sep = ".")
    
    events <- events %>%
      select(-victimDamageReceived)
    
    
  }
  
  return(list(events, assist_parts, victim_damage_dealt, victim_damage_recieved))
  
}


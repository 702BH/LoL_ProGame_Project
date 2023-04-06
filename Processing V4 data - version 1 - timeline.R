# Processing V4 data - version 1 - timeline
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


test_v4_timeline_1 <- readRDS("Testv4_timeline_1.RData")

test_v4_timeline_2 <- readRDS("Testv4_timeline_2.RData")

test_v4_timeline_1$stats_title
test_v4_timeline_2$stats_title



function_0_timeline_v4 <- function(list_to_processes){
  
  processed_data <- lapply(list_to_processes, function_1_timeline_v4)
  
  parts_table <- lapply(processed_data, function(x) x[[1]])
  events_table <- lapply(processed_data, function(x) x[[2]])
  
  final_parts <- bind_rows(parts_table)
  final_events <- bind_rows(events_table)
  
  return(list(final_parts, final_events))
  
}


processed_test_v4_timeline <- function_0_timeline_v4(test_list_v4_timeline)

length(processed_test_v4_timeline)

processed_parts <- processed_test_v4_timeline[[1]]

processed_events <- processed_test_v4_timeline[[2]]

dim(processed_parts)
dim(processed_events)

colnames(processed_parts)
colnames(processed_events)

processed_parts$title







function_1_timeline_v4 <- function(raw_data){
  
  raw_data_file <- raw_data
  
  json_data <- fromJSON(raw_data$content)
  
  ## Participant frames
  participant_frames <- function_2_timeline_v4(json_data)
  
  # add game id and title
  participant_frames <- function_3(raw_data_file, json_data, participant_frames)
  
  # add team ids
  participant_frames <- function_3_timeline(participant_frames)
  
  
  ## Events
  events <- function_3_timeline_v4(json_data)
  
  events <- function_3(raw_data_file, json_data, events)
  
  
  return(list(participant_frames, events))
  
}

test_timeline_v4 <- function_1_timeline_v4(test_v4_timeline_1)


parts_table <- test_timeline_v4[[1]]
frames_table <- test_timeline_v4[[2]]

dim(parts_table)
dim(frames_table)


function_2_timeline_v4 <- function(data){
  
  frames <- data$frames$timestamp
  part_frames <- data$frames$participantFrames
  
  bind_frames <- cbind(part_frames, frames)
  
  frames_pivot <- pivot_longer(bind_frames, 1:10)
  
  participant_frames_final <- frames_pivot %>%
    unnest(value) %>%
    unnest(position)
  
  return(participant_frames_final)
  
}


function_3_timeline_v4 <- function(data){
  
  events <- tibble(events = data$frames$events)
  
  
  events_tidy <- events %>%
    unnest(events) %>%
    unnest(position)
  
  return(events_tidy)
}


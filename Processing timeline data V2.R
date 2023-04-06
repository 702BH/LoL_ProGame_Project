# Processing timeline data V2
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
test_content_timeline <- readRDS("processing_test_timeline_data.RData")




test_v5_timeline_2 <-readRDS("TestV5_timeline_2.RData")



function_0_timeline <- function(list_to_processes){
  
  # old method
  processed_data <- lapply(list_to_processes, function_1_timeline)
  
  parts_table <- lapply(processed_data, function(x) x[[1]])
  events_table <- lapply(processed_data, function(x) x[[2]])
  
  final_parts <- bind_rows(parts_table)
  final_events <- bind_rows(events_table)
  
  return(list(final_parts, final_events))
  
}

processed_test_2 <- function_0_timeline(test_list2_timeline)

length(processed_test_2)

processed_parts <- processed_test_2[[1]]

processed_events <- processed_test_2[[2]]

dim(processed_parts)
dim(processed_events)

colnames(processed_parts)
colnames(processed_events)

function_1_timeline <- function(raw_data){
  
  raw_data_file <- raw_data
  
  # old method
  json_data <- fromJSON(raw_data$content)
  
  ## Participant frames
  participant_frames <- function_2_timeline(json_data)
  
  # add game id and title
  participant_frames <- function_3(raw_data_file, json_data, participant_frames)
  
  # add team ids
  participant_frames <- function_3_timeline(participant_frames)
  
  
  ## events
  events <- function_4_timeline(json_data)
  
  events <- function_3(raw_data_file, json_data, events)
  
  return(list(participant_frames, events))
  
}

test_timeline_1 <- function_1_timeline(test_content_timeline)


test_combined <- function_1_timeline(test_content_timeline)
length(test_combined)

parts_table <- test_combined[[1]]
frames_table <- test_combined[[2]]

dim(parts_table)
dim(frames_table)

# take in the converted json, create needed strucutre
function_2_timeline <- function(data){
  
  frames <- data$frames$timestamp
  part_frames <- data$frames$participantFrames
  
  bind_frames <- cbind(part_frames, frames)
  
  frames_pivot <- pivot_longer(bind_frames, 1:10)
  
  participant_frames_final <- frames_pivot %>%
    unnest(value) %>%
    unnest(championStats) %>%
    unnest(damageStats) %>%
    unnest(position)
  
  return(participant_frames_final)
  
}

# manually create the teamid
function_3_timeline <- function(data){
  
  
  participant_data <- data %>%
    mutate(teamId = case_when(
      participantId <= 5 ~ 100,
      TRUE ~ 200
    ))
  
  return(participant_data)
  
}

function_4_timeline <- function(data){
  
  events <- tibble(events = data$frames$events)
  
  
  events_tidy <- events %>%
    unnest(events) %>%
    unnest(position) %>%
    unnest_wider(victimDamageDealt, names_sep = ".") %>%
    unnest_wider(victimDamageReceived, names_sep = ".")
  
  return(events_tidy)
}

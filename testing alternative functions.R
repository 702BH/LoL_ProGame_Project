# testing alternative functions

#"fucntion 2"


test_V5_1


function_2 <- function(data){
  
  # select participants
  # remove "perks" = runes info
  # unnest the challenges 
  
  ## old 
  # participants <- data$participants
  # participants <- participants %>%
  #   select(-perks) %>%
  #   unnest_wider(challenges, names_sep = ".")
  
  # new
  participants <- data$participants
  participants <- participants %>%
    select(-perks) %>%
    unnest(challenges, names_sep = ".")
  
  
  return(participants)
  
  
}



test_2 <- function_2(fromJSON(test_V5_1$content))

view(test_2)



# get the participants structure
function_2_v4 <- function(data){
  
  participants <- data$participants
  
  participants <- participants %>%
    select(-c(timeline, participantId)) %>%
    unnest(stats)
  
  return(participants)
  
  
}

# new function
function_2_defensive <- function(data){
  
  
  
}

# first ever reccursive function.

function_unnest_recursively <- function(df){
  
  df_cols <- df %>%
    select_if(is.data.frame) %>%
    names()
  
  if (length(df_cols) > 0){
    df <- df %>%
      unnest(all_of(df_cols), names_sep = ".") %>%
      function_unnest_recursively()
  }
  
  return(df)
  
}



test <- fromJSON(test_V5_1$content)
test$participants$championName
test_participatns <- test$participants
no_perks <- test_participatns %>%
  select(-perks)

test_recrs <- function_unnest_recursively(test_participatns)



view(test_recrs)

testv4 <- fromJSON(test_v4_stats_1$content)

test_participatnsv4 <- testv4$participants


test_recrsv4 <- function_unnest_recursively(test_participatnsv4)

view(test_recrsv4)

test_flat <- flatten(test_participatnsv4)

test_flat

test_flatv4 <- flatten(test_participatns)

test_flatv4$

sapply(no_perks, class)

df_cols <- no_perks %>%
  select_if(is.data.frame) %>%
  names()
df_cols


a <- no_perks %>%
  mutate(across(all_of(df_cols), function_unnest_recursively))




#### FUNCTION 3_v4 - replacing id championnames
function_3_v4 <- function(data){
  
  data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
  
  return(data)
}

test_3_func <- function(data) {
  
  if("championName" %in% colnames(data)){
    return(data)
  }else{
    
    data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
    return(data)
  }
  
}

test3v5 <- test_3_func(test_participatns)
test3v5$championName

test3v4 <- test_3_func(test_participatnsv4)



### FUNCTION 4_v4
function_4_v4 <- function(json_data, processing_data){
  
  part_identities <- json_data$participantIdentities %>%
    unnest(player) %>%
    select(-profileIcon)
  
  sep_identities <- function_4(part_identities)
  
  identities_join <-left_join(processing_data, sep_identities, by = "participantId")
  
  return(identities_join)
  
}



test_4_func <- function(json_data, partiticpant_data){
  
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

testv4 <- fromJSON(test_v4_stats_1$content)

names(testv4)

test_participatnsv4 <- testv4$participants

test <- fromJSON(test_V5_1$content)
test_participatns <- test$participants

v4_4 <- test_4_func(testv4, test_participatnsv4)
v4_4$summoner_name

v5_4 <- test_4_func(test, test_participatns)


### function_5_v4 / function_10?
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

test_teams <- test$teams

view(test_teams)
test_compare <- function_10(test)
test_flat <- flatten(test_teams)
view(test_flat$bans)

colnames(test_compare)
colnames(test_flat)


# v4 testing
v4_team_test <- testv4$teams
test_v4_flat <- flatten(v4_team_test)
colnames(test_v4_flat)

v4_func_teams <- function_5_v4(testv4)
colnames(v4_func_teams)

test_func_5_10 <- function(data){
  
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

v5_10 <- test_func_5_10(test)
v4_10 <- test_func_5_10(testv4)

view(v4_10)







##### TIMELINE

# testing data
test_v4_timeline_1 <- readRDS("Testv4_timeline_1.RData")
test_v5_timeline_2 <-readRDS("TestV5_timeline_2.RData")



#Function_2_timeline_v4
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



test_v4_func2 <- function_2_timeline_v4(fromJSON(test_v4_timeline_1$content))


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


test_v5_func2 <- function_2_timeline(fromJSON(test_v5_timeline_2$content))
colnames(test_v5_func2)


test_function_2_timeline <- function(data){
  
  frames <- data$frames$timestamp
  part_frames <- data$frames$participantFrames
  
  bind_frames <- cbind(part_frames, frames)
  
  frames_pivot <- pivot_longer(bind_frames, 1:10)
  
  participant_frames_final <- frames_pivot %>%
    flatten()
  
  return(participant_frames_final)
  
}

test_v4 <- test_function_2_timeline(fromJSON(test_v4_timeline_1$content))
test_v5 <- test_function_2_timeline(fromJSON(test_v5_timeline_2$content))

colnames(test_v5)
dim(test_v5_func2)



# function_3_timeline_v4 / 4
function_3_timeline_v4 <- function(data){
  
  events <- tibble(events = data$frames$events)
  
  
  events_tidy <- events %>%
    unnest(events) %>%
    unnest(position)
  
  return(events_tidy)
}

test_v4_func2 <- function_3_timeline_v4(fromJSON(test_v4_timeline_1$content))
dim(test_v4_func2)


function_4_timeline <- function(data){
  
  events <- tibble(events = data$frames$events)
  
  
  events_tidy <- events %>%
    unnest(events) %>%
    unnest(position) %>%
    unnest_wider(victimDamageDealt, names_sep = ".") %>%
    unnest_wider(victimDamageReceived, names_sep = ".")
  
  return(events_tidy)
}
test_v5_func2 <- function_4_timeline(fromJSON(test_v5_timeline_2$content))
dim(test_v5_func2)




test_function_events_timeline <- function(data){
  events <- tibble(events = data$frames$events)
  
  events_tidy <- flatten(events)
  
  return(events_tidy)
}

test_v4 <- test_function_events_timeline(fromJSON(test_v4_timeline_1$content))
test_v5 <- test_function_events_timeline(fromJSON(test_v5_timeline_2$content))

dim(test_v4)
dim(test_v5)

# testing
v4_json <- fromJSON(test_v4_timeline_1$content)
v4_test_flat <- tibble(events = v4_json$frames$events)

v4_flat <- flatten(v4_test_flat)
view(v4_flat)



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

test_v4_recrs <- function_unnest_recursively_timeline(v4_test_flat)

view(test_v4_recrs)

dim(test_v4_recrs)

df_cols <- v4_test_flat %>%
  select_if(is.list) %>%
  names()
df_cols

test_me <- v4_test_flat %>%
  unnest(events)

view(test_me)

class(v4_test_flat)

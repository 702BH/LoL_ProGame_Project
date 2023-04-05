# Script for processing metadata

library(tidyverse)
library(httr)
library(jsonlite)


###### Stats Data
raw_stats_data <- read_csv("post_game_stats_raw.csv", col_names = TRUE)


# attempting to structure the input data
my_list <- lapply(seq_len(nrow(raw_stats_data)), function(i){
  list(stats_title = raw_stats_data[i, "stats_title"], content =raw_stats_data$content[i])
  
})

my_list[[1]]$content

# testing data
test_content <- my_list[[1]]
length(test_content)

# save this testing data
saveRDS(test_content, file = "processing_test_data.RData")

# read in
test_content <- readRDS("processing_test_data.RData")

test_content$stats_title

## defining function
# first lets define each element separate

# convert the JSON file
json_data <- fromJSON(test_content$content)

json_data$teams$teamId

class(json_data$participants$teamId)

# testing for v4 or v5
if(str_detect(test_content$stats_title, "V4")){
  print("V4")
}else{
  print("V5")
}

#### proccessing 
### participants
participants <- json_data$participants

view(participants$perks)

## remove perks from participants (FOR NOW)
participants <- participants %>%
  select(-perks)

colnames(participants)
class(participants$assists)
class(participants$summonerName)
view(participants)

## calculating aggregate stats for team level analysis (for now)
# check integers?
inter_test <- sapply(participants, is.integer)
is_teamid <- names(participants) == "teamId"
select_cols <- inter_test | is_teamid

only_ints <- participants[, select_cols]
view(only_ints$teamId)

aggregate_sum <- only_ints %>%
  group_by(teamId) %>%
  summarise_all(sum)

## addgame id
participants <- cbind(gameid = json_data$gameId, participants)

## check for logicals
logicals <- sapply(participants, is.logical)

# convert logicals 
participants[,logicals] <- lapply(participants[,logicals], as.integer)


## team name and summoner name
participants <- participants %>%
  separate(summonerName, c("team_short", "summoner_name"), sep = " ", remove = FALSE)

participants$summoner_name


## item names
participants$item0

# getting item names
item_json <- fromJSON("http://ddragon.leagueoflegends.com/cdn/13.6.1/data/en_US/item.json")
item_data <- item_json$data
item_data$`1001`
names(item_data)
str(item_data)

item_names <- lapply(item_json$data, function(x) x$name)

item_names
names(item_names)

item_df <- data.frame(item_id = names(item_names), item_name = unlist(item_names), row.names = NULL)

view(item_df)

# add missing names from ornn items (????
item_df <- rbind(item_df, data.frame(item_id = 0, item_name = "No Item"))

item_df$item_id <- as.numeric(item_df$item_id)


view(item_df$item_name)

item_df[1:5,]

# write to file
write.csv(item_df, "item_name_id.csv",  row.names = FALSE)


# replace the id values with the names
participants[, grep("^item\\d+$", colnames(participants))] <- map(participants[, grep("^item\\d+$", colnames(participants))], function(col){
  item_df[match(col, item_df$item_id), "item_name"]
}) 


## summonmer spells
# getting summoner spell

summoner_spell_json <- fromJSON("http://ddragon.leagueoflegends.com/cdn/13.6.1/data/en_US/summoner.json")
summoner_spell_json$data$SummonerBarrier$name

# get the name and the key
names_and_keys <- lapply(summoner_spell_json$data, function(spell){
  name <- spell$name
  key <- spell$key
  list(name = name, key = key)
})

names_and_keys
summoner_spells_df <- map_df(names_and_keys, ~as.data.frame(.x), .default = NA)

view(summoner_spells_df)

write.csv(summoner_spells_df, "summoner_spell_id_name.csv",  row.names = FALSE)

# replace the values
participants[, grep("^spell.*Id$", colnames(participants))] <- map(participants[, grep("^spell.*Id$", colnames(participants))], function(col){
  summoner_spells_df[match(col, summoner_spells_df$key), "name"]
}) 

participants[, grep("^spell.*Id$", colnames(participants))]


## adding champion names
# get names and key
champ_json <- fromJSON("http://ddragon.leagueoflegends.com/cdn/13.6.1/data/en_US/champion.json")

champ_data <- champ_json$data

champ_data$Aatrox$key

champ_names_and_keys <- lapply(champ_data, function(champ){
  name <- champ$name
  key <- champ$key
  list(name = name, key = key)
})
champ_names_df <- map_df(champ_names_and_keys, ~as.data.frame(.x), .default = NA)
view(champ_names_df)

write.csv(champ_names_df, "champ_id_name.csv",  row.names = FALSE)

# replace the values (dont need to do for v5)
participants$championName

## team id?
participants$teamId


### TEAMS
teams <- json_data$teams
view(teams$objectives$baron$first)

team_objectives <- teams$objectives
view(team_objectives)

unnest_obj <- do.call(data.frame, team_objectives)

test <- teams %>%
  select(-objectives)

test <- cbind(test, unnest_obj)

tester <- test %>%
  unnest_wider(bans) %>%
  select(-pickTurn)

view(tester)

tester$championId <- lapply(tester$championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])

# convert logcials (win)
cols_team_select <- sapply(tester, is.logical)

tester[,cols_team_select] <- lapply(tester[,cols_team_select], as.integer)
view(tester)

## add game id
tester <- cbind(gameid = json_data$gameId, tester)
view(tester)

# add stats title
tester <- cbind(full_title = test_content$stats_title, tester)
view(tester)













##### TIMELINE DATA
raw_timeline_data <- read_csv("post_game_timeline_raw.csv", col_names = TRUE)


# attempting to structure the input data
my_list_timeline <- lapply(seq_len(nrow(raw_timeline_data)), function(i){
  list(stats_title = raw_timeline_data[i, "stats_title"], content =raw_timeline_data$content[i])
  
})

my_list_timeline[[1]]$content

# testing data
test_content_timeline <- my_list_timeline[[1]]
length(test_content_timeline)

# save this testing data
saveRDS(test_content_timeline, file = "processing_test_timeline_data.RData")

# read in
test_content_timeline <- readRDS("processing_test_timeline_data.RData")

test_content_timeline$stats_title

# convert the JSON file
json_data_timeline <- fromJSON(test_content_timeline$content)

flat_json <- flatten(json_data_timeline$frames)

flat_json$events

test_participants <- json_data_timeline$participants


## Participant frames
frames <- json_data_timeline$frames$timestamp

part_frames <- json_data_timeline$frames$participantFrames

# bind these together
frames_bind <- cbind(part_frames, frames)

# pivot longer
frames_pivot <- pivot_longer(frames_bind, 1:10)

participant_frames_final <- frames_pivot %>%
  unnest(value) %>%
  unnest(championStats) %>%
  unnest(damageStats) %>%
  unnest(position)

# add teamid, gameid, title
# teamid
participant_frames_final <- participant_frames_final %>%
  mutate(teamId = case_when(
    participantId <= 5 ~ 100,
    TRUE ~ 200
  ))

view(participant_frames_final$title)

#gameid
participant_frames_final$gameId <- json_data_timeline$gameId

# title
participant_frames_final$title <- test_content_timeline$stats_title

dim(participant_frames_final)


### EVENTS
## explore
events <- json_data_timeline$frames$events


# events row 1
events <- tibble(events = json_data_timeline$frames$events)

view(events)

colnames(events)

test2 <- events %>%
  unnest(events) %>%
  unnest(position) %>%
  unnest_wider(victimDamageDealt, names_sep = ".") %>%
  unnest_wider(victimDamageReceived, names_sep = ".")

test2_filter <- test2 %>%
  filter(type == "CHAMPION_KILL")

unique(test2$type)

view(test2)

view(test2_filter)

dim(test2)

str(test2)



### testing
events_1 <- events$events[5]

view(events_1)

## testing doing them seperate
test2_filter <- test2 %>%
  filter(type == "CHAMPION_KILL") %>%
  select_if(~ !any(is.na(.)))

colnames(test2_filter)

view(test2_filter)

test_more <- events %>%
  unnest(events) %>%
  unnest(position) %>% 
  filter(type == "CHAMPION_KILL") %>%
  select_if(~ !any(is.na(.)))

colnames(test_more)

view(test_more)

head(test_more, 1)

test_more_1 <- test_more %>%
  unnest_wider(victimDamageDealt, names_sep = ".") %>%
  unnest_wider(victimDamageReceived, names_sep = ".")

head(test_more_1)

str(test_more_1)

test_more_2 <- flatten(test_more_1)

view_test <- test_more_1 %>%
  select(victimDamageReceived.name, victimDamageReceived.spellName)

head(view_test, 1)

view_test

view(test_more_1)

view(test_more_2)

str(test_more_1)

view(test_more)

# adding title and game id





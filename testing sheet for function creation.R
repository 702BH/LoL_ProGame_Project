# testing sheet for function creation
#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)


###### PArticipants data

## function 3 adding relationship information

test_content <- readRDS("processing_test_data.RData")
json_data_testing <- fromJSON(test_content$content)

participants_t <- json_data_testing$participants
participants_t <- participants_t %>%
  select(-perks) %>%
  unnest_wider(challenges, names_sep = ".")

colnames(participants_t)

# add game id
participants_t$gameId <- json_data_testing$gameId

participants_t$gameId

# add title
participants_t$title <- test_content$stats_title$stats_title

participants_t$title


# seperate summoner name and team name short?
participants_t <- participants_t %>%
  separate(summonerName, c("team_short", "summoner_name"), sep = " ", remove = FALSE)

participants_t$summoner_name


# adding item names
participants_t$item0

participants_t[, grep("^item\\d+$", colnames(participants_t))] <- unlist(map(participants_t[, grep("^item\\d+$", colnames(participants_t))], function(col){
  item_helper[match(col, item_helper$item_id), "item_name"]
})) 

participants_t$item0

class(participants_t$item0)

view(participants_t[, grep("^item\\d+$", colnames(participants_t))])


participants_t[, grep("^item\\d+$", colnames(participants_t))] <- apply(participants_t[, grep("^item\\d+$", colnames(participants_t))], 2, function(col){
  unlist(item_helper[match(col, item_helper$item_id), "item_name"])
})

participants_t$item0

class(participants_t$item0$item_name)

participants_t$item0

participants_t[, grep("^item\\d+$", colnames(participants_t))]

class(participants_t$item0)

class(participants_t[, grep("^item\\d+$", colnames(participants_t))])


str(participants_t$item0)

view(participants_t[, grep("^item\\d+$", colnames(participants_t))])


test_content$stats_title$stats_title
participants_t$teamId

# join info from games data
games_filtered <- games_table %>%
  filter(StatsPage == test_content$stats_title$stats_title) %>%
  select(Gamelength, Patch, StatsPage, team_name, reference_team_key, side, result)

view(games_filtered)

test_part_join_games <- left_join(participants_t, games_filtered, by = c("title" = "StatsPage", "teamId" = "reference_team_key"))

colnames(test_part_join_games)

view(test_part_join_games$team_name)


test_part_join_games_2 <- left_join(participants_t, games_table, by = c("title" = "StatsPage", "teamId" = "reference_team_key"))

view(test_part_join_games_2$team_name)
colnames(test_part_join_games_2)



## getting a second V5 for testing
view(games_table)
test_content$stats_title$stats_title
title <- "V5 data:ESPORTSTMNT03 3087499"

post_game_list_stats <- list()
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = title,
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)

api_content <- content(match_api_data)

titles_and_content <- lapply(api_content$query$pages, function(page){
  title <- page$title
  content <- page$revisions[[1]][[1]]$main$`*`
  list(stats_title = title, content = content)
})

test_df <- map_df(titles_and_content, ~as.data.frame(.x), .default = NA)

post_game_list_stats[[1]] <- test_df

post_game_df <- bind_rows(post_game_list_stats)

my_list <- lapply(seq_len(nrow(post_game_df)), function(i){
  list(stats_title = post_game_df[i, "stats_title"], content =post_game_df$content[i])
  
})

my_list
test_content_2 <- my_list[[1]]$content
test_content_2$stats_title

saveRDS(test_content_2, file = "TestV5_2.RData")

test_V5_1 <- list(stats_title = test_content$stats_title$stats_title, content = test_content$content)

test_V5_1$stats_title

# creating list?
test_list <- c(test_V5_1, test_V5_2)

test_V5_1

test_list[[1]]

test_V5_1$stats_title

test_v5_2$stats_title

test_V5_1$content

str(test_list)

test_list2 <- list(test_V5_1, test_v5_2)

str(test_list2)




##### TEAM DATA
processed_data <- lapply(test_list2, function_1)

str(processed_data)

json_data <- fromJSON(test_content$content)
teams <- json_data$teams

team_objectives <- teams$objectives

unnest_obj <- do.call(data.frame, team_objectives)

test <- teams %>%
  select(-objectives)

test <- cbind(test, unnest_obj)

tester <- test %>%
  unnest_wider(bans, names_sep = ".") %>%
  select(-bans.pickTurn)

tester$bans.championId <- lapply(tester$bans.championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])

view(tester)

tester$bans.championId

tester$bans.championId


tester$bans.championId <- sapply(tester$bans.championId, paste, collapse = ",")

view(tester)

tester2 <- tester %>%
  separate(bans.championId, into = paste0("Ban", 1:5), sep = ",")


view(tester2)

view(tester2)

colnames(tester2) <- paste0("Ban", 1:5)

view(tester2)

view(test3)

















##### TIMELINE DATA
test_content_timeline <- readRDS("processing_test_timeline_data.RData")

json_data_timeline <- fromJSON(test_content_timeline$content)

## Participant frames
frames <- json_data_timeline$frames$timestamp

frames

part_frames <- json_data_timeline$frames$participantFrames

view(part_frames)

# bind these together
frames_bind <- cbind(part_frames, frames)

# pivot longer
frames_pivot <- pivot_longer(frames_bind, 1:10)

participant_frames_final <- frames_pivot %>%
  unnest(value) %>%
  unnest(championStats) %>%
  unnest(damageStats) %>%
  unnest(position)

view(participant_frames_final)

colnames(participant_frames_final)


## events
events <- tibble(events = json_data_timeline$frames$events)


events_tidy <- events %>%
  unnest(events) %>%
  unnest(position) %>%
  unnest_wider(victimDamageDealt, names_sep = ".") %>%
  unnest_wider(victimDamageReceived, names_sep = ".")


unique(events_tidy$type)

dim(events_tidy)

which(sapply(events_tidy, function(x) is.data.frame(x)))



### get another timeline data for testing!
test_content_timeline$stats_title$stats_title


view(games_table)
title <- "V5 data:ESPORTSTMNT03 3087499/Timeline"

post_game_list_timeline <- list() 
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = title,
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)

api_content <- content(match_api_data)

titles_and_content_timeline <- lapply(api_content$query$pages, function(page){
  title <- page$title
  content <- page$revisions[[1]][[1]]$main$`*`
  list(stats_title = title, content = content)
})

test_df <- map_df(titles_and_content_timeline, ~as.data.frame(.x), .default = NA)

post_game_list_timeline[[1]] <- test_df

post_game_df <- bind_rows(post_game_list_timeline)

my_list_timeline <- lapply(seq_len(nrow(post_game_df)), function(i){
  list(stats_title = post_game_df[i, "stats_title"], content =post_game_df$content[i])
  
})

my_list_timeline[[1]]$content

# testing data
test_content_timeline_2 <- my_list_timeline[[1]]
test_content_timeline_2
saveRDS(test_content_timeline_2, file = "TestV5_timeline_2.RData")



test_V5_timeline_1 <- list(stats_title = test_content_timeline$stats_title$stats_title, content = test_content_timeline$content)

test_v5_timeline_2$stats_title
test_V5_timeline_1$stats_title

test_list2_timeline <- list(test_V5_timeline_1, test_v5_timeline_2)




























#### V4 Data
### Stats page

# get testing data
title1 <- "V4 data:ESPORTSTMNT04 1480651"
title2 <- "V4 data:ESPORTSTMNT04 1480665"

post_game_list_stats <- list()
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = title2,
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)

api_content <- content(match_api_data)

titles_and_content <- lapply(api_content$query$pages, function(page){
  title <- page$title
  content <- page$revisions[[1]][[1]]$main$`*`
  list(stats_title = title, content = content)
})

test_df <- map_df(titles_and_content, ~as.data.frame(.x), .default = NA)

post_game_list_stats[[1]] <- test_df

post_game_df <- bind_rows(post_game_list_stats)

post_game_df$stats_title

my_list <- lapply(seq_len(nrow(post_game_df)), function(i){
  list(stats_title = post_game_df[i, "stats_title"], content =post_game_df$content[i])
  
})

my_list[[1]]$stats_title
length(my_list)

test_v4_stats_2 <- my_list[[1]]
test_v4_stats_2$stats_title

saveRDS(test_v4_stats_2, file = "Testv4_stats_2.RData")



# processing test data




# Final Script for obtaining the metadata
setwd("C:/Users/house/Desktop/Getting into Modelling/Scripts")
library(tidyverse)
library(httr)
library(jsonlite)

# Metadata - Leagues
api_base_url <- "https://lol.fandom.com/api.php"

# in this param, we must include the Tournemants table so 
# we do not select leagues with which we have no data
query_param <- list(
  action = "cargoquery",
  limit = 'max',
  tables = "Leagues=L, Tournaments=T",
  join_on = "L.League = T.League",
  fields = "L.League, L.Region, L.League_Short",
  where = "L.IsOfficial = 'Yes' AND L.Level = 'Primary' AND T.DateStart IS NOT NULL",
  group_by = "L.League",
  format = "json"
)

league_api_data <- GET(api_base_url, query = query_param)


api_content <- content(league_api_data)

api_content

content_df <- map_df(api_content$cargoquery, ~as.data.frame(.x))
colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')
view(content_df)

write.csv(content_df, "Leagues.csv",  row.names = FALSE)


# Metadata - Tournaments
league_data <- read_csv("Leagues.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))
view(league_data)

final_df_list <- list()

for (i in league_data$League){
  
  query_param <- list(
    action = "cargoquery",
    limit = '500',
    tables = "Tournaments=T",
    fields = "T.League, T.Name, T.DateStart",
    where = paste0("T.League=","'",i,"'", "AND T.DateStart IS NOT NULL"),
    order_by = "T.League, T.DateStart DESC",
    format = "json"
  )
  
  temp_api_data <- GET(api_base_url, query = query_param)
  
  
  temp_api_content <- content(temp_api_data)
  
  content_df <- map_df(temp_api_content$cargoquery, ~as.data.frame(.x))
  colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')

  final_df_list[[i]] <- content_df
  
  
  Sys.sleep(5)
}

final_df_list

final_df <- bind_rows(final_df_list)

dim(final_df)

view(final_df)

# messed up the order by
# fix in post
temp_df <- final_df %>%
  arrange(League, desc(DateStart))

write.csv(temp_df, "Tournaments.csv",  row.names = FALSE)



# metadata about all games for all tournaments
tournaments_data <- read_csv("Tournaments.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

view(tournaments_data)

games_list <- list()

### UNDER WORK, NEEDS TO BE TESTED
for(i in tournaments_data$Name[1:2]){
  
  query_param <- list(
    action = "cargoquery",
    limit = '1',
    tables = "ScoreboardGames=SG, PostgameJsonMetadata=PJ",
    fields = "SG.Tournament, SG.DateTime_UTC, SG.OverviewPage, SG.Team1, SG.Team2, SG.WinTeam, SG.RiotGameId, SG.RiotVersion, SG.RiotPlatformGameId, PJ.StatsPage, PJ.TimelinePage",
    where = paste0("SG.Tournament=", "'",i, "'"),
    join_on = "SG.RiotPlatformGameId =PJ.RiotPlatformGameId",
    order_by = "SG.DateTime_UTC",
    format = "json"
  )
  
  temp_api_data <- GET(api_base_url, query = query_param)
  
  
  temp_api_content <- content(temp_api_data)
  
  content_df <- map_df(temp_api_content$cargoquery, ~as.data.frame(.x), .default = NA)
  colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')
  
  games_list[[i]] <- content_df
  
  
  Sys.sleep(5)
}


# further testing
names <- tournaments_data$Name[1:10]
names

query_param <- list(
  action = "cargoquery",
  limit = '1',
  tables = "ScoreboardGames=SG, PostgameJsonMetadata=PJ",
  fields = "SG.Tournament, SG.DateTime_UTC, SG.OverviewPage, SG.Team1, SG.Team2, SG.WinTeam, SG.RiotGameId, SG.RiotVersion, SG.RiotPlatformGameId, PJ.StatsPage, PJ.TimelinePage",
  where = paste0("SG.Tournament=", "'",names[2], "'"),
  join_on = "SG.RiotPlatformGameId =PJ.RiotPlatformGameId",
  order_by = "SG.DateTime_UTC",
  format = "json"
)

temp_api_data <- GET(api_base_url, query = query_param)


temp_api_content <- content(temp_api_data)
temp_api_content$cargoquery

str(temp_api_content$cargoquery)

res <- map_depth(temp_api_content$cargoquery, 3, ~ifelse(is.null(.x), NA, .x))
res

vec_depth(temp_api_content$cargoquery)


test_df <- do.call(rbind, lapply(temp_api_content$cargoquery, rbind)) %>%
  as.data.frame()
view(test_df)
test_df

content_df <- map_df(temp_api_content$cargoquery, ~as.data.frame(.x))
colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')

view(content_df)

final_df_list

final_df <- bind_rows(final_df_list)

dim(final_df)

view(final_df)




### Calling random tournament
query_param <- list(
  action = "cargoquery",
  limit = '1',
  tables = "ScoreboardGames=SG, PostgameJsonMetadata=PJ",
  fields = "SG.Tournament, SG.DateTime_UTC, SG.OverviewPage, SG.Team1, SG.Team2, SG.WinTeam, SG.RiotPlatformId, SG.RiotGameId, SG.RiotPlatformGameId, PJ.StatsPage, PJ.TimelinePage",
  where = "SG.Tournament= 'LCS 2020 Spring Playoffs'",
  join_on = "SG.RiotPlatformGameId =PJ.RiotPlatformGameId",
  order_by = "SG.DateTime_UTC",
  format = "json"
)

temp_api_data <- GET(api_base_url, query = query_param)

temp_api_content <- content(temp_api_data)
temp_api_content


# actually calling!
# first filter tournemants for games from 2019:
class(tournaments_data$DateStart)
view(tournaments_data$DateStart)

test <- as.Date(tournaments_data$DateStart, "%d/%m/%Y")
test

tournaments_data$DateStart <- as.Date(tournaments_data$DateStart, "%d/%m/%Y")

tournaments_data_filtered <- tournaments_data %>%
  filter(DateStart >= '2019-01-01')

view(tournaments_data_filtered)

dim(tournaments_data_filtered)
dim(tournaments_data)

# test

games_list <- list()

for(i in tournaments_data_filtered$Name){
  
  query_param <- list(
    action = "cargoquery",
    limit = 'max',
    tables = "ScoreboardGames=SG, PostgameJsonMetadata=PJ",
    fields = "SG.Tournament, SG.DateTime_UTC,SG.Team1, SG.Team2, SG.WinTeam, SG.Gamelength, SG.GameId, SG.MatchId, SG.Patch, SG.RiotPlatformGameId, PJ.StatsPage, PJ.TimelinePage",
    where = paste0("SG.Tournament=", "'",i, "'"),
    join_on = "SG.RiotPlatformGameId =PJ.RiotPlatformGameId",
    order_by = "SG.DateTime_UTC",
    format = "json"
  )
  
  temp_api_data <- GET(api_base_url, query = query_param)
  
  
  temp_api_content <- content(temp_api_data)
  res <- map_depth(temp_api_content$cargoquery, 3, ~ifelse(is.null(.x), NA, .x))
  
  content_df <- map_df(res, ~as.data.frame(.x), .default = NA)
  colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')
  
  games_list[[i]] <- content_df
  
  
  Sys.sleep(5)
}

games_list
games_df <- bind_rows(games_list)

view(games_df)

write.csv(games_df, "Games.csv",  row.names = FALSE)



### getting postgame jsons
games_data <- read_csv("Games.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

view(games_data)

# use first row as test
tester <- games_data[1,]
tester

api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = paste0(tester$StatsPage, "|", tester$TimelinePage),
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)


api_content <- content(match_api_data)
str(api_content$query)

list_item <- api_content$query$revisions[[1]][[1]]$slots[[1]]$main$`*`
list_item

api_content$query$pages[[1]]$revisions[[1]][[1]]$main$`*`


# testing splitting the titles into 500s:
titles <- games_data$StatsPage
dim(titles)
class(titles)

group_factor <- cut(seq_along(titles), breaks = 500, labels = FALSE)

title_sublists <- split(titles, group_factor)

title_sublists$`1`


# trying again
num_sublists <- ceiling(length(titles)/500)

title_sublists_2 <- lapply(1:num_sublists, function(i){
  start_index <- (i-1) * 500 + 1
  end_index <- min(start_index + 499, length(titles))
  titles[start_index:end_index]
})

title_sublists_2[[1]][1]

test_first_sublist_string <- paste(title_sublists_2[[1]], collapse = "|")
test_first_sublist_string

test2 <- paste(title_sublists_2[[1]][1:50], collapse = "|")
test2


# so what happens if we provide this to the api?
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = test2,
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)

api_content <- content(match_api_data)

api_content$error

str(api_content$query)


# ok so we know now the limit is 50. so lets do it. and then the same for the timeline data
num_sublists <- ceiling(length(titles)/50)

title_sublists_2 <- lapply(1:num_sublists, function(i){
  start_index <- (i-1) * 50 + 1
  end_index <- min(start_index + 49, length(titles))
  titles[start_index:end_index]
})
title_sublists_2

sublist_strings <- lapply(title_sublists_2, function(sublist){
  paste(sublist, collapse = "|")
})

sublist_strings

test_sublist_string <- unlist(sublist_strings)
test_sublist_string

# now test if we can create the output we want from the input
str(api_content$query)

titles_and_content <- lapply(api_content$query$pages, function(page){
  title <- page$title
  content <- page$revisions[[1]][[1]]$main$`*`
  list(stats_title = title, content = content)
})


str(titles_and_content)

view(test_df)

test_df <- map_df(titles_and_content, ~as.data.frame(.x), .default = NA)
class(test_df$content)

post_game_list_stats <- list()


for (i in test_sublist_string){
  
  query_param <- list(
    action = "query",
    format = "json",
    prop = "revisions",
    titles = i,
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
  
  post_game_list_stats[[i]] <- test_df
  
  
  Sys.sleep(5)
  
  
}

post_game_df <- bind_rows(post_game_list_stats)


write.csv(post_game_df, "post_game_stats_raw.csv",  row.names = FALSE)

colnames(post_game_df)

view(post_game_df[1,])

# testing if we can read it
raw_stats_data <- read_csv("post_game_stats_raw.csv", col_names = TRUE)

colnames(raw_stats_data)

raw_stats_data[1,]

view(raw_stats_data[1,])


# ok think it works? think we can read it. 

# testing if we can analyse?
test_data <- raw_stats_data[1,]

api_final <- fromJSON(test_data$content)

test <- test_data %>%
  unnest_longer(content)
view(test)

view(api_final$participants)


## doing the above again for timelines
timeline <- games_data %>%
  filter(UTC >= '2022-01-01')

timeline <- timeline$TimelinePage

timeline

length(timeline)

old_timeline <- games_data$TimelinePage
length(old_timeline)

num_sublists_timeline <- ceiling(length(timeline)/5)

timeline_sublists <- lapply(1:num_sublists_timeline, function(i){
  start_index <- (i-1) * 5 + 1
  end_index <- min(start_index + 4, length(timeline))
  timeline[start_index:end_index]
})
timeline_sublists

sublist_strings_timeline <- lapply(timeline_sublists, function(sublist){
  paste(sublist, collapse = "|")
})

sublist_strings_timeline

test_sublist_string_timeline <- unlist(sublist_strings_timeline)
test_sublist_string_timeline


# testing the subscrips
test3 <- paste(test_sublist_string_timeline[1], collapse = "|")
test3


# so what happens if we provide this to the api?
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = test3,
  rvprop = "content",
  rvslots = "main"
)

match_api_data <- GET(api_base_url_post, query = query_param)

api_content <- content(match_api_data)

str(api_content$query)

title <- api_content$query$pages$title
title

titles_and_content_timeline <- lapply(api_content$query$pages, function(page){
  title <- page$title
  content <- page$revisions[[1]][[1]]$main$`*`
  list(stats_title = title, content = content)
})


titles_and_content_timeline

### NOTE THIS IS FOR 2022 ONWARDS ONLY!!!!! Need to do the other years another time!


post_game_list_timeline <- list()

api_base_url_post <- "https://lol.fandom.com/api.php"

count <- 0

for( i in test_sublist_string_timeline[1:10]){
  count <- count + 1
  
  print(paste0("completed ", count, " of: ", length(test_sublist_string_timeline[1:10])))
}


for (i in test_sublist_string_timeline){
  count <- count + 1
  
  query_param <- list(
    action = "query",
    format = "json",
    prop = "revisions",
    titles = i,
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
  
  post_game_list_timeline[[i]] <- test_df
  
  print(paste0("completed ", count, " of: ", length(test_sublist_string_timeline)))
  Sys.sleep(5)
  
  
}


post_game_df_timeline <- bind_rows(post_game_list_timeline)


write.csv(post_game_df_timeline, "post_game_timeline_raw.csv",  row.names = FALSE)

colnames(post_game_df_timeline)






### Downloading champion information


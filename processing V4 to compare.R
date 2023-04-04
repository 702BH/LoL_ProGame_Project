# processing V4 to compare



#### v4

### Stats data

# query data
api_base_url_post <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = "V4 data:ESPORTSTMNT04 1480651",
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

# convert the JSON file
json_data <- fromJSON(test_df$content)

# testing for v4 or v5
if(str_detect(test_df$stats_title, "V4")){
  print("V4")
}else{
  print("V5")
}

colnames(participants)

## participants
participants <- json_data$participants

str(participants)

view(participants$timeline)

colnames(participants$stats)

teams <- json_data$teams

colnames(teams)

test <- json_data$participantIdentities

str(test)
colnames(test)

### timline data
query_param <- list(
  action = "query",
  format = "json",
  prop = "revisions",
  titles = "V4 data:ESPORTSTMNT04 1480651/Timeline",
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

json_data <- fromJSON(test_df$content)


test <- tibble(json_data$frames$events)

str(test)
colnames(test)

view(test$`json_data$frames$events`[6])
# testing sheet for function creation
#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)



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

participants_t$item0$

participants_t[, grep("^item\\d+$", colnames(participants_t))]

class(participants_t$item0)

class(participants_t[, grep("^item\\d+$", colnames(participants_t))])


str(participants_t$item0)

view(participants_t[, grep("^item\\d+$", colnames(participants_t))])

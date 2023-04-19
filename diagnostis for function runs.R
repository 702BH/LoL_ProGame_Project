# diagnostic for function runs

# run 1 = error:
# "V5 data:ESPORTSTMNT03 2883666"

error_data <- stats_data_raw %>%
  filter(stats_title == "V5 data:ESPORTSTMNT03 2883666")

error_json <- fromJSON(error_data$content)

view(error_json$teams)

# error game in games table
games_table_error <- games_table %>%
  filter(StatsPage == "V5 data:ESPORTSTMNT03 2883666")

view(games_table_error)



# run 2 error:
# "V4 data:ESPORTSTMNT03 1143691"
error_data <- stats_data_raw %>%
  filter(stats_title == "V4 data:ESPORTSTMNT03 1143691")

error_json <- fromJSON(error_data$content)

view(error_json$participants$championId)

error_participants <- error_json$participants


games_table_error <- games_table %>%
  filter(StatsPage == "V4 data:ESPORTSTMNT03 1143691")

view(games_table_error)


# testing the champion id replacement
data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
error_participants$championId <- sapply(error_participants$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))

error_participants$championId

class(error_participants$championId)

view(champ_names_df)


#try with other title
# V4 data:ESPORTSTMNT03 1143689

error_data2 <- stats_data_raw %>%
  filter(stats_title == "V4 data:ESPORTSTMNT03 1143689")

error_json2 <- fromJSON(error_data2$content)


error_participants2 <- error_json2$participants


class(error_participants2$championId)

error_participants2$championId <- sapply(error_participants2$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))

error_participants2$championId

str(error_data2)


# lets put it through the whole process
error_data <- stats_data_raw %>%
  filter(stats_title == "V5 data:ESPORTSTMNT03 2883666")

str(error_data)

test <- function_stats_data(error_data)
test[[1]]$championId


# further addressing the error
error_data <- stats_data_raw %>%
  filter(stats_title == "V4 data:ESPORTSTMNT02 2344275")

test <- function_stats_data(error_data)
test[[1]]$championId

error_json <- fromJSON(error_data$content)

# v4 stats
v5_index <- unlist(lapply(my_list, function(x) str_detect(x$stats_title, "V5 ")))
timeline_index <- unlist(lapply(my_list, function(x) str_detect(x$stats_title, "/Timeline")))

v4_stats_list <- combined_list[!v5_index & !timeline_index]

length(v4_stats_list)

t <- v4_stats_list[7650]
t

function_output <- function_process_combine(v4_stats_list)
t <- v4_stats_list[7605]
t[[1]]$stats_title

function_output <- function_process_combine(v4_stats_list)

error_matches <- function_output$error_matches

errors <-error_matches[!sapply(error_matches,is.null)]
length(errors)


# further diagnosing
# v4 stats
v5_index <- unlist(lapply(my_list, function(x) str_detect(x$stats_title, "V5 ")))
timeline_index <- unlist(lapply(my_list, function(x) str_detect(x$stats_title, "/Timeline")))

v4_stats_list <- my_list[!v5_index & !timeline_index]

length(v4_stats_list)

function_output <- function_process_combine(v4_stats_list)







# exploring the errors caused in V4 data
v5_index <- unlist(lapply(stats_list, function(x) str_detect(x$stats_title, "V5 ")))
timeline_index <- unlist(lapply(stats_list, function(x) str_detect(x$stats_title, "/Timeline")))

v4_stats_list <- stats_list[!v5_index & !timeline_index]

function_output <- function_process_combine(v4_stats_list)
dim(function_output$v4_participant_stats)

errors <- function_output$error_matches
length(errors)

errors_notnull <-errors[!sapply(errors,is.null)]

length(errors_notnull)

error_1 <- errors_notnull[[1]]
error_1$stats_title
error_1

error_1_content <- fromJSON(error_1$content)

names(error_1_content)

error_1_parts <- error_1_content$participants

colnames(error_1_parts)

# steps of function
flat <- function_flatten_participants(error_1_content)

flat_add <- function_add_relationships(flat, error_1, error_1_content)

flat_sep <- function_sep_names(error_1_content, flat_add)

flat_item <- function_replace_id_values(flat_sep, "item\\d+$", item_helper)

# replace summoner spell ids with names
flat_spell <- function_replace_id_values(flat_item, "spell.*Id$", summoner_spell_helper)

flat_champs <- function_replace_champs(flat_spell)

flat_0 <- function_na_to_0(flat_champs)

# convert logicals
flat_logic <- function_logicals_to_int(flat_0)

flat_logic$championId
class(flat_logic$championId)

dim(flat_logic)

str(v4_stats_list)





index <- which(sapply(v4_stats_list, function(x) x$stats_title) == "V4 data:ESPORTSTMNT01 1881943")
index
v4_stats_list[3679]
v4_stats_list[3678]
str(v4_stats_list[3678:3679])

test_list2 <- v4_stats_list[3678:3679]

test_output <- function_process_combine(test_list2)

view(test_output$v4_team_stats)

test_output$error_matches

test_list3 <- v4_stats_list[3679]
test_output3 <- function_process_combine(test_list3)


# exploring the teams process
content <- fromJSON(test_list3[[1]]$content)

teams <- content$teams

teams$bans

teams_unnest <- teams %>%
  unnest_wider(bans, names_sep = ".")

view(teams_unnest)
length(teams$bans)
lengths(teams$bans)

teams3 <- NULL

if(any(lengths(teams$bans) == 0)){
  teams3 <- teams2 %>%
    select(-bans) %>%
    add_column(bans.championId = NA)
}

view(teams3)

content2 <- fromJSON(v4_stats_list[3678][[1]]$content)

teams2 <- content2$teams

teams2$bans
  
view(teams2)
length(teams2$bans)

teams_unnest2 <- teams2 %>%
  unnest_wider(bans, names_sep = ".")

view(teams_unnest2)




##### RUN - JUST v4 Error diagnositcs
function_output <- function_process_combine(v4_stats_list)

function_output$error_matches

errors_notnull <-function_output$error_matches[!sapply(function_output$error_matches,is.null)]

length(errors_notnull)

str(errors_notnull)

error_1 <- errors_notnull[[1]]
error_1

error_1_content <- fromJSON(error_1$content)
error_1_content$

# does it contain participants and teams? yes
names(error_1_content)


# going through the function processes

# steps of function
flat <- function_flatten_participants(error_1_content)

flat_add <- function_add_relationships(flat, error_1, error_1_content)

flat_sep <- function_sep_names(error_1_content, flat_add)

flat_item <- function_replace_id_values(flat_sep, "item\\d+$", item_helper)

# replace summoner spell ids with names
flat_spell <- function_replace_id_values(flat_item, "spell.*Id$", summoner_spell_helper)

flat_champs <- function_replace_champs(flat_spell)

flat_0 <- function_na_to_0(flat_champs)

# convert logicals
flat_logic <- function_logicals_to_int(flat_0)

colnames(flat_logic)

# filter games table
games_table_error <- games_table %>%
  filter(StatsPage == "V4 data:VN 4029131695")

view(games_table_error)


# getting v5 data for comparisons
v5_stats_list <- stats_list[v5_index & !timeline_index]


str(v5_stats_list)

str(test)

test <- function_process_combine(v5_stats_list[1])
colnames(test$v5_participant_stats)



# testing on errors


# testing again
function_output <- function_process_combine(v4_stats_list)
options(warn=2)


# error occured in splitting the name
error_data <- stats_data_raw %>%
  filter(stats_title == "V4 data:ESPORTSTMNT02 1682108")

error_json <- fromJSON(error_data$content)

view(error_json$participantIdentities)


flat <- function_flatten_participants(error_json)

flat_add <- function_add_relationships(flat, error_1, error_1_content)

flat_sep <- function_sep_names(error_json, flat_add)

part_identities <- error_json$participantIdentities

view(part_identities)

stats_list <- lapply(seq_len(nrow(error_data)), function(i){
  list(stats_title = error_data$stats_title[i], content =error_data$content[i])
  
})

function_output <- function_process_combine(stats_list)
function_output$v4_participant_stats$team_name





## testing again with the modifications to seperate to extract:
function_output <- function_process_combine(v4_stats_list)
options(warn=2)

index <- which(sapply(v4_stats_list, function(x) x$stats_title) == "V4 data:ESPORTSTMNT03 1119121")
index
length(v4_stats_list)



error_data <- stats_data_raw %>%
  filter(stats_title == "V4 data:ESPORTSTMNT03 1119121")

error_json <- fromJSON(error_data$content)


# steps of function
flat <- function_flatten_participants(error_json)

flat_add <- function_add_relationships(flat, error_1, error_1_content)

flat_sep <- function_sep_names(error_1_content, flat_add)

flat_item <- function_replace_id_values(flat_sep, "item\\d+$", item_helper)

# replace summoner spell ids with names
flat_spell <- function_replace_id_values(flat_item, "spell.*Id$", summoner_spell_helper)

flat_champs <- function_replace_champs(flat_spell)

flat_0 <- function_na_to_0(flat_champs)

# convert logicals
flat_logic <- function_logicals_to_int(flat)


logicals <- sapply(flat, is.logical)
logicals_cols <- which(logicals)

print(logicals)

which(logicals == TRUE)

view(flat$stats.win)

class(flat$stats.win)

flat[,logicals_cols] <- lapply(flat[,logicals_cols], as.integer)

function_logicals_to_int <- function(processing_data){
  
  ## check for logicals
  logicals <- sapply(processing_data, is.logical)
  
  # convert logicals 
  processing_data[,logicals] <- lapply(processing_data[,logicals], as.integer)
  
  return(processing_data)
  
}
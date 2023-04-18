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

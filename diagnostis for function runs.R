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

# obtaining testing data
library(tidyverse)
library(lubridate)
# reading in data

# stats
v5_part_stats <- read_csv("v5_part_stats.csv", col_names = TRUE)
v5_team_stats <- read_csv("v5_team_stats.csv", col_names = TRUE)

# timeline
v5_events_all <- read_csv("v5_events_all.csv", col_names = TRUE)
v5_events_assit_parts <- read_csv("v5_events_assit_parts.csv", col_names = TRUE)
v5_events_victim_damage_dealt <- read_csv("v5_events_victim_damage_dealt.csv", col_names = TRUE)
v5_events_victim_damage_recieved <- read_csv("v5_events_victim_damage_recieved.csv", col_names = TRUE)
v5_timeline_parts <- read_csv("v5_timeline_parts.csv", col_names = TRUE)



games_table <- read_csv("games_transformed.csv", col_names = TRUE)


head(games_table$UTC)
class(games_table$UTC)


games_table$UTC <- as.Date(games_table$UTC, "%Y-%m-%d", tz = "UTC")

# filter for year 2022 and leagues of interest
games_data_filtered <- games_table %>%
  filter(UTC >= '2019-01-01' & UTC <='2023-01-01') %>%
  filter(str_detect(Tournament, "LCK|LCS|LEC"))







# when using memory stick:
# read games table
games_table <- read_csv("D:/games_transformed.csv", col_names = TRUE)

head(games_table$UTC)
class(games_table$UTC)

# converting date
test_date <- as.Date(games_table$UTC, "%Y-%m-%d", tz = "UTC")

test_date

games_table$UTC <- as.Date(games_table$UTC, "%Y-%m-%d", tz = "UTC")

unique(games_table$Tournament)

# filter for year 2022 and leagues of interest
games_data_filtered <- games_table %>%
  filter(year(UTC) == 2022) %>%
  filter(str_detect(Tournament, "LCK|LCS|LEC"))

dim(games_data_filtered)
unique(games_data_filtered$Tournament)


write.csv(games_data_filtered, "D:/practice_data_2022.csv",  row.names = FALSE)


# obtain only LEC
lec_data <- games_data_filtered %>%
  filter(str_detect(Tournament, "LEC"))

unique(lec_data$Tournament)

write.csv(lec_data, "D:/practice_data_lec_2022.csv",  row.names = FALSE)




# Use the obtained testing data to filter other data frames
# set dir to local data
setwd("C:/Users/house/Desktop/Getting into Modelling/Data not on github")

# test data
practice_data <- read_csv("practice_data_2022.csv", col_names = TRUE)
practice_lec <- read_csv("practice_data_lec_2022.csv", col_names = TRUE)


# stats
v5_part_stats <- read_csv("v5_part_stats.csv", col_names = TRUE)
v5_team_stats <- read_csv("v5_team_stats.csv", col_names = TRUE)

# timeline
v5_events_all <- read_csv("v5_events_all.csv", col_names = TRUE)
v5_events_assit_parts <- read_csv("v5_events_assit_parts.csv", col_names = TRUE)
v5_events_victim_damage_dealt <- read_csv("v5_events_victim_damage_dealt.csv", col_names = TRUE)
v5_events_victim_damage_recieved <- read_csv("v5_events_victim_damage_recieved.csv", col_names = TRUE)
v5_timeline_parts <- read_csv("v5_timeline_parts.csv", col_names = TRUE)




# v5 part stats
practice_data$RiotPlatformGameId
v5_part_stats$title
test_join <- left_join(practice_data, v5_part_stats, by = c("RiotPlatformGameId" = "title"))

unique(test_join$Tournament.x)
dim(test_join)

list_of_df <- list(v5_part_stats = v5_part_stats, v5_team_stats = v5_team_stats, v5_events_all,
                   v5_events_assit_parts, v5_events_victim_damage_dealt,
                   v5_events_victim_damage_recieved, v5_timeline_parts)
name(v5_part_stats)

list_of_df[[1]]

df <- list_of_df[[1]]
deparse(substitute(df))

# iteration
for(i in seq_along(list_of_df)){
  
  df <- list_of_df[[i]]
  join_name <- paste0("join_", deparse(substitute(df)))
  print(join_name)
  #joined_data <- left_join(practice_data, df, by = c("RiotPlatformGameId" = "title"))
  #write.csv(joined_data, file = paste0(join_name, ".csv"), row.names = FALSE)
}

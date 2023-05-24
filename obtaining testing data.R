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

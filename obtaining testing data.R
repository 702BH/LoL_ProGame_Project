# obtaining testing data

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

games_table$UTC <- as.Date(tournaments_data$DateStart, "%d/%m/%Y")

tournaments_data_filtered <- tournaments_data %>%
  filter(DateStart >= '2019-01-01')

head(games_table$UTC)
class(games_table$UTC)

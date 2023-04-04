## Transforming the games table
raw_game_data <- read_csv("Games.csv", col_names = TRUE)

head(raw_game_data, 2)


## Pivot longer so each team is on one row
game_data_pivot <- pivot_longer(raw_game_data, cols = starts_with("Team"), names_to = "team",
                                values_to = "team_name")

head(game_data_pivot$team,2 )

view(game_data_pivot)

## Add reference key for team_100, team_200 for joins
game_data_pivot <- game_data_pivot %>%
  mutate(reference_team_key = case_when(
    team ==  "Team1" ~ 100,
    team == "Team2" ~ 200
    
  ))


## Add side(based on assumptions
game_data_pivot_side <- game_data_pivot %>%
  mutate(side = case_when(
    team ==  "Team1" ~ "Blue",
    team == "Team2" ~ "Red"
    
  ))


# Add result



view(game_data_pivot_side)

write.csv(post_game_df_timeline, "post_game_timeline_raw.csv",  row.names = FALSE)
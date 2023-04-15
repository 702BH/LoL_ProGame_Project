# testing function


testing_function <- function_process_combine(combined_test_list)

processed_v5_stats_parts <- testing_function[[1]]

processed_v5_stats_teams <- testing_function[[2]]


## error in timeline data recursive function
test_input_timeline

test_input_timeline[[1]]$stats_title

test_timeline <- lapply(test_input_timeline, function_timeline_data)


# test input 1 - events
json_content <- fromJSON(test_input_timeline[[1]]$content)

events <- tibble(events = json_content$frames$events)

events2 <- function_unnest_recursively_timeline(events)

view(events)

df_cols <- test_events %>%
  select_if(is.list) %>%
  names()

df_cols

if (length(df_cols) > 0){
  df <- df %>%
    unnest(all_of(df_cols), names_sep = ".") %>%
    function_unnest_recursively_timeline()
}


test_events2 <- test_events %>%
  unnest(all_of(df_cols))

view(test_events)


events_tidy <- events %>%
  unnest(events) %>%
  select(-assistingParticipantIds) %>%
  unnest(position) %>%
  unnest(victimDamageDealt, names_sep = ".") %>%
  unnest(victimDamageReceived, names_sep = ".") 

view(events_tidy)

df_cols <- events_tidy %>%
  select_if(is.list) %>%
  names()

df_cols

test_events3 <- events_tidy %>%
  unnest(all_of(df_cols))

head(events_tidy$victimDamageDealt.name)

events_testing <- events %>%
  unnest(events) %>%
  unnest_wider(victimDamageDealt, names_sep = ".") %>%
  unnest_wider(victimDamageReceived, names_sep = ".")

dim(events_testing)

view(events_testing)

#test_unlist
test_df <- events_testing

test_df$assistingParticipantIds <- unlist(test_df$assistingParticipantIds)

test_df$assistingParticipantIds

df_cols <- events_testing %>%
  select_if(is.list) %>%
  names()

df_cols
events_testing$assistingParticipantIds
dim(events_testing)


df_cols <- test_events2 %>%
  select_if(is.list) %>%
  names()

df_cols


test_events2 <- test_events2 %>%
  unnest(all_of(df_cols))


unnest_all <- function(data) {
  nested_cols <- sapply(data, function(col) is.list(col) || is.data.frame(col))
  for (i in which(nested_cols)) {
    col_name <- names(data)[i]
    if (is.list(data[[i]])) {
      data <- unnest_wider(data, col_name)
      data <- unnest_all(data)
    } else if (is.data.frame(data[[i]])) {
      data <- unnest_longer(data, col_name)
      data <- unnest_all(data)
    }
  }
  return(data)
}

test <- unnest_all(events)





















#### testing timeline processing
events_test <- events %>%
  unnest(events)

unique(events_test$type)

events_kill <- events_test %>%
  filter(type == "CHAMPION_KILL")

view(events_kill)

events_kill_recieved <- events_kill %>%
  unnest(victimDamageReceived, names_sep = ".")

view(events_kill_recieved)





events_kill_dealt <- events_kill %>%
  select(victimDamageDealt) %>%
  unnest(victimDamageDealt, names_sep = ".")

view(events_kill_dealt)



df_cols <- events_kill_dealt %>%
  select_if(is.list) %>%
  names()

df_cols

test_kill_dealt2 <- events_kill_dealt %>%
  unnest(all_of(df_cols))

view(test_kill_dealt2)



# processing kills
view(events_kill)

# recieved
events_kill_recieved_no_wide <- events_kill %>%
  unnest(victimDamageReceived, names_sep = ".")

view(events_kill_recieved_no_wide)

colnames(events_kill_recieved_no_wide)


# both
events_kill$
events_kill_all <- events_kill %>%
  select(timestamp, victimDamageDealt, victimDamageReceived) %>%
  unnest(victimDamageDealt, names_sep = ".") %>%
  unnest(victimDamageReceived, names_sep = ".")

view(events_kill_all)
colnames(events_kill_all)

# calculate totals
totals_recieved <- events_kill_all %>%
  group_by(timestamp, victimDamageDealt.name, victimDamageReceived.name) %>%
  summarise(total_magic_damage = sum(victimDamageReceived.magicDamage),
            total_physical_damage = sum(victimDamageReceived.physicalDamage),
            total_true_damage = sum(victimDamageReceived.trueDamage))

view(totals_recieved)  






# New tests, 12/04/2023
testing_function <- function_process_combine(test_list)

# error:
# Error in function_process_combine(test_list) : 
# object 'v4_events_all' not found

# forgot to initialise empty lists

testing_function <- function_process_combine(test_list)

# so we should have 15
length(testing_function)
# we do

# since this testing data only contained V5, V5 should all be filled.
view(testing_function$v5_timeline_parts)

# the timeline data has not worked

# testing the function controller:
processed_data <- lapply(test_input_timeline, function_timeline_data)

length(processed_data)


parts_table <- lapply(processed_data, function(x) x[[1]])
events_table <- lapply(processed_data, function(x) x[[2]])

final_parts <- bind_rows(parts_table)

view(final_parts)

events_all <- lapply(events_table, function(x) x[[1]])
events_assit_parts <- events_table[2]
events_victim_damage_dealt <- events_table[3]
events_victim_damage_recieved <- events_table[4]

final_parts <- bind_rows(parts_table)
final_events_all <- bind_rows(events_all)
final_events_assit_parts <- bind_rows(events_assit_parts)
final_events_victim_damage_dealt <- bind_rows(events_victim_damage_dealt)
final_events_victim_damage_recieved <- bind_rows(events_victim_damage_recieved)

view(final_events_all)


t <- processed_data[[1]]
str(t)

str(events_table)


# think fixed, test again
testing_function <- function_process_combine(test_list)

view(testing_function$v5_events_victim_damage_recieved)



# testing with v4 data
testing_function <- function_process_combine(test_list_w_v4)

str(test_list_w_v4)

view(testing_function$v4_events_victim_damage_dealt)



# testing only v4 timeline
test_v4_timeline_data <- function_process_combine(v4_timeline)

# no warning messages

# testing only v4 stats
test_v4_stats_data <- function_process_combine(test_input_v4_stats)

# error in function process combine, and warning, expected 5 pieces. 
view(test_v4_stats_data$v4_participant_stats)

which(apply(test_input_v4_stats, 1, function(x) length(x) != 5))

str(test_input_v4_stats)



# testing the teams flatten function
test_input_v4_stats[[7]]$stats_title
test_json <- test_input_v4_stats[[7]]$content

json <- fromJSON(test_json)

view(json$participantIdentities)

teams <- json$teams

teams <- flatten(teams)

view(teams)

test <- teams %>%
  unnest_wider(bans, names_sep = ".")

view(test)

teams <- teams %>%
  unnest_wider(bans, names_sep = ".") %>%
  select(-bans.pickTurn)

test_flatten <- function_flatten_teams(json)

games_table$StatsPage

games_filtered <- games_table %>%
  filter(StatsPage == "V4 data:ESPORTSTMNT03 2093869")

view(games_filtered)

teams$bans.championId <- lapply(teams$bans.championId, function(x) champ_names_df$name[match(x, champ_names_df$key)])

teams$bans.championId <- sapply(teams$bans.championId, paste, collapse = ",")


teams <- teams %>%
  separate(bans.championId, into = paste0("Ban", 1:5), sep = ",", fill = "right")
view(teams)


# test again
test_v4_stats_data <- function_process_combine(test_input_v4_stats)

view(test_v4_stats_data$v4_team_stats)




# testing
# want to see all tables have all the required relational information
testing_function <- function_process_combine(test_list_w_v4)






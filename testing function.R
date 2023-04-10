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

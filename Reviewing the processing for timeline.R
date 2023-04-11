# Reviewing the processing for timeline
# Libraries
library(tidyverse)
library(httr)
library(jsonlite)

### TEST DATA
test_content_timeline <- readRDS("processing_test_timeline_data.RData")


json_content <- fromJSON(test_content_timeline$content)

events <- tibble(events = json_content$frames$events)

# Unnest events
test_e <- events %>%
  unnest(events)

view(test_e)

df_cols <- test_e %>%
  select_if(is.list) %>%
  names()

df_cols

unique(test_e$type)

test_e_1 <- test_e %>%
  unnest_auto(victimDamageDealt)

view(test_e_1)


test_remove_nested <- test_e %>%
  select(-all_of(df_cols))

view(test_remove_nested)


## testing "exploding" the data

# current dim
dim(test_e)
# 1586, 34

# expand position
test_e_position <- test_e %>%
  unnest(position, names_sep = ".")

# dim
dim(test_e_position)
# 1586, 35

# unnest victimdamagedealt
test_e_damge_dealt <- test_e_position %>%
  unnest_wider(victimDamageDealt, names_sep = ".")
view(test_e_damge_dealt)

dim(test_e_damge_dealt)

df_cols <- test_e_damge_dealt %>%
  select_if(is.list) %>%
  names()
df_cols <- df_cols[str_detect(df_cols, "Dealt")]
df_cols


test_events3 <- test_e_damge_dealt %>%
  unnest_longer(all_of(df_cols))

view_filter <- test_events3 %>%
  filter(type == "CHAMPION_KILL")

view(view_filter)


test_e_damage_recieved <- test_events3 %>%
  unnest_wider(victimDamageReceived, names_sep = ".")

df_cols <- test_e_damage_recieved %>%
  select_if(is.list) %>%
  names()
df_cols <- df_cols[str_detect(df_cols, "\\.")]
df_cols

test_events4 <- test_e_damage_recieved %>%
  unnest_longer(all_of(df_cols))

view_filter <- test_events4 %>%
  filter(type == "CHAMPION_KILL")

view(view_filter)



# further testing
test_10 <- unnest_longer(unnest_longer(test_e, victimDamageDealt), victimDamageReceived)

view(test_10)


## try chatgpt solution:
new_df <- test_e %>%
  mutate(event_id = row_number())

view(new_df)


# damage dealt
df_dd <- new_df %>%
  select(-victimDamageReceived)

df_dd_u <- df_dd %>%
  unnest(victimDamageDealt, names_sep = ".", keep_empty = FALSE)

view(df_dd_u)


test_join <- left_join(b_df, df_dd_u, by = "event_id")

view(test_join)

# base df
b_df <- new_df %>%
  select(-c(victimDamageReceived, victimDamageDealt))


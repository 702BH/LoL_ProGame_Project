# Running the function

# run 1 - 15/04/2023
# data - combined_list

function_output <- function_process_combine(combined_list)

function_process_combine(combined_list)


# run 2 - 17/04/2023
# data - combined_list
function_output <- function_process_combine(combined_list)


# run 3 - 17/04/2023
function_output <- function_process_combine(combined_list)

# run 4 - 20/04/2023 - after fixing the issues in V4 - stats
function_output <- function_process_combine(combined_list)

errors_notnull <- function_output$error_matches[!sapply(function_output$error_matches,is.null)]

length(errors_notnull)
str(errors_notnull)

# saving outputs
saveRDS(errors_notnull, file = "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/error_matches.RData")

v5_part_stats <- function_output$v5_participant_stats

t <- sapply(v5_part_stats, function(x) is.list(x))

which(t == TRUE)

v5_part_stats <- v5_part_stats %>%
  select(-perks.styles)

write.csv(v5_part_stats, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_part_stats.csv", row.names = FALSE)

v5_team_stats <- function_output$v5_team_stats
write.csv(v5_team_stats, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_team_stats.csv", row.names = FALSE)


v4_participant_stats <- function_output$v4_participant_stats 
write.csv(v4_participant_stats, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v4_participant_stats.csv", row.names = FALSE)

v4_team_stats <- function_output$v4_team_stats 
write.csv(v4_team_stats, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v4_team_stats.csv", row.names = FALSE)

v5_timeline_parts <- function_output$v5_timeline_parts 
write.csv(v5_timeline_parts, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_timeline_parts.csv", row.names = FALSE)

dim(v5_timeline_parts)

v5_events_all <- function_output$v5_events_all 
write.csv(v5_events_all, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_events_all.csv", row.names = FALSE)

v5_events_assit_parts <- function_output$v5_events_assit_parts 
write.csv(v5_events_assit_parts, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_events_assit_parts.csv", row.names = FALSE)

v5_events_victim_damage_dealt <- function_output$v5_events_victim_damage_dealt 
write.csv(v5_events_victim_damage_dealt, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_events_victim_damage_dealt.csv", row.names = FALSE)

v5_events_victim_damage_recieved <- function_output$v5_events_victim_damage_recieved 
write.csv(v5_events_victim_damage_recieved, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v5_events_victim_damage_recieved.csv", row.names = FALSE)


v4_timeline_parts <- function_output$v4_timeline_parts 
write.csv(v4_timeline_parts, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v4_timeline_parts.csv", row.names = FALSE)

dim(v4_timeline_parts)

v4_events_all <- function_output$v4_events_all 
write.csv(v4_events_all, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v4_events_all.csv", row.names = FALSE)

v4_events_assit_parts <- function_output$v4_events_assit_parts 
write.csv(v4_events_assit_parts, "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/v4_events_assit_parts.csv", row.names = FALSE)

saveRDS(function_output, file = "C:/Users/house/Desktop/Getting into Modelling/Data Processing Tables/function_output.RData")

# Putting the functions together


## Input Data Preparation

# Stats page
stats_data_raw <- read_csv("D:/post_game_stats_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

# attempting to structure the input data

my_list <- lapply(seq_len(nrow(stats_data_raw)), function(i){
  list(stats_title = stats_data_raw$stats_title[i], content =stats_data_raw$content[i])
  
})


test_input <- my_list[1:10]

length(test_input)

test_input[[4]]$stats_title


function_combine_functions <- function(input_list){
  
  
  # Detecting new lists
  v5_index <- unlist(lapply(input_list, function(x) str_detect(x$stats_title, "V5")))
  timeline_index <- unlist(lapply(input_list, function(x) str_detect(x$stats_title, "/Timeline")))
  
  v5_stats_list <- input_list[v5_index & !timeline_index]
  v5_timeline_list <- input_list[v5_index & timeline_index]
  
  v4_stats_list <- input_list[!v5_index & !timeline_index]
  v4_timeline_list <- input_list[!v5_index & timeline_index]
  
  
  
  # V5
  # stats
  if(length(v5_stats_list) > 0){
    
    v5_stats <- function_0(v5_stats_list, games_table)
    v5_participant_stats <- v5_stats[[1]]
    v5_team_stats <- v5_stats[[2]]
    
  }else{
    v5_participant_stats <- list()
    v5_team_stats <- list()
  }
  
  
  # timeline
  if(length(v5_timeline_list) > 0){
    v5_timeline <- function_0_timeline(v5_timeline_list)
    v5_timeline_parts <- v5_timeline[[1]]
    v5_timeline_events <- v5_timeline[[2]]
    
  }else{
    v5_timeline_parts <- list()
    v5_timeline_events <- list()
  }
  
  
  # v4
  # stats
  if(length(v4_stats_list) > 0){
    
    v4_stats <- function_0_v4(v4_stats_list, games_table)
    v4_participant_stats <- v5_stats[[1]]
    v4_team_stats <- v4_stats[[2]]
  }else{
    v4_participant_stats <-list()
    v4_team_stats <- list()
  }
  
  
  # timeline
  if(length(v4_timeline_list) > 0){
    v4_timeline <- function_0_timeline_v4(v4_timeline_list)
    v4_timeline_parts <- v4_timeline[[1]]
    v4_timeline_events <- v4_timeline[[2]]
  }else{
    v4_timeline_parts <- list()
    v4_timeline_events <- list()
  }
  
  
  return(list(v5_participant_stats, v5_team_stats, v4_participant_stats, v4_team_stats, v5_timeline_parts,
              v5_timeline_events, v4_timeline_parts, v4_timeline_events))
  
  
  }


testing_function <- function_combine_functions(test_input)


processed_v5_stats_parts <- testing_function[[1]]

processed_v5_stats_teams <- testing_function[[2]]

dim(processed_v5_stats_parts)

view(processed_v5_stats_parts$result)

# for testing purposes
function_combine_check_stats <- function(data){
  
  if(!str_detect(data$stats_title, "/Timeline")){
    print("yes")
  }else{
    print("no")
  }
  
}

lapply(test_input, function_combine_functions)

test_input[[1]]$stats_title

# so now we have seperated into timeline and stats data
# now we want to seperate these into v4_timeline and v4_stats etc

v5_index <- unlist(lapply(test_input, function(x) str_detect(x$stats_title, "V5")))
timeline_index <- unlist(lapply(test_input, function(x) str_detect(x$stats_title, "/Timeline")))

v5_stats_list <- test_input[v5_index & !timeline_index]
v5_timeline_list <- test_input[v5_index & timeline_index]

v4_stats_list <- test_input[!v5_index & !timeline_index]
v4_timeline_list <- test_input[!v5_index & timeline_index]

length(v4_timeline_list)

v5_index

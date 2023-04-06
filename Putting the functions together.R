# Putting the functions together


## Input Data Preparation

# Stats page
stats_data_raw <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/post_game_stats_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

timeline_test_1 <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/csv_timeline_partition1.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))





# attempting to structure the input data

my_list <- lapply(seq_len(nrow(stats_data_raw)), function(i){
  list(stats_title = stats_data_raw$stats_title[i], content =stats_data_raw$content[i])
  
})

my_list_timeline <- lapply(seq_len(nrow(timeline_test_1)), function(i){
  list(stats_title = timeline_test_1$stats_title[i], content =timeline_test_1$content[i])
  
})

test_input <- my_list[1:10]

length(test_input)

test_input_timeline <- my_list_timeline[1:10]

test_input[[4]]$content

test_input_list[[1]]$stats_title


combined_test_list <- c(test_input, test_input_timeline)

length(combined_test_list)

function_combine_functions <- function(input_list){
  
  error_titles <- c()
  
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
    
    error_titles <- c(error_titles, v5_stats[[3]])
    
  }else{
    v5_participant_stats <- list()
    v5_team_stats <- list()
  }
  
  
  # timeline
  if(length(v5_timeline_list) > 0){
    v5_timeline <- function_0_timeline(v5_timeline_list)
    v5_timeline_parts <- v5_timeline[[1]]
    v5_timeline_events <- v5_timeline[[2]]
    
    error_titles <- c(error_titles, v5_timeline[[3]])
    
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
    
    error_titles <- c(error_titles, v4_stats[[3]])
    
  }else{
    v4_participant_stats <-list()
    v4_team_stats <- list()
  }
  
  
  # timeline
  if(length(v4_timeline_list) > 0){
    v4_timeline <- function_0_timeline_v4(v4_timeline_list)
    v4_timeline_parts <- v4_timeline[[1]]
    v4_timeline_events <- v4_timeline[[2]]
    
    error_titles <- c(error_titles, v4_timeline[[3]])
    
  }else{
    v4_timeline_parts <- list()
    v4_timeline_events <- list()
  }
  
  
  return(list(v5_participant_stats = v5_participant_stats, v5_team_stats = v5_team_stats, v4_participant_stats = v4_participant_stats, v4_team_stats = v4_team_stats, v5_timeline_parts = v5_timeline_parts,
              v5_timeline_events = v5_timeline_events, v4_timeline_parts = v4_timeline_parts, v4_timeline_events = v4_timeline_events,
              error_matches = error_titles))
  
  
  }


testing_function <- function_combine_functions(test_input)

view(testing_function$v5_participant_stats)


processed_v5_stats_parts <- testing_function[[1]]

processed_v5_stats_teams <- testing_function[[2]]

dim(processed_v5_stats_parts)

view(processed_v5_stats_parts$result)


# testing combined stats and timeline
testing_function_combined <- function_combine_functions(combined_test_list)

testing_function_combined


# testing the error?
testing_function_combined_errors <- function_combine_functions(combined_test_list_error)

test_error <- function_combine_functions(combined_test_list_error)


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






### creating a bad list to test error?

error_df <- data.frame(
  name = c("John", "Mary", "Tom"),
  age = c(25, 30, 35),
  city = c("New York", "Los Angeles", "Chicago")
)

json_error <- toJSON(error_df)

my_error_list <- list(
  stats_title = "v5",
  content = "")

length(my_error_list)

my_error_list$stats_title

combined_test_list_error <-list(my_error_list)

combined_test_list_error <- c(test_input, test_input_timeline, my_error_list)

length(combined_test_list_error)

t <- read_json(my_error_list$content)

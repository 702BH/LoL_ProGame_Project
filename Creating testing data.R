# Creating testing data
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

combined_test_list <- c(test_input, test_input_timeline)
length(combined_test_list)

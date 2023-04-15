# Creating testing data
stats_data_raw <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/post_game_stats_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

timeline_test_1 <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/csv_timeline_partition1.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))


# from mem stick
stats_data_raw <- read_csv("D:/post_game_stats_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

timeline_test_1 <- read_csv("D:/csv_timeline_partition1.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))


timeline_test_4 <- read_csv("D:/csv_timeline_partition4.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))


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


saveRDS(combined_test_list, file = "stats_timeline_test_list.RData")

test_list <-readRDS("stats_timeline_test_list.RData")



# attempting to get some V4 data too

v4_stats <- stats_data_raw %>%
  filter(str_detect(stats_title, "V4 "))


my_list_v4_stats <- lapply(seq_len(nrow(v4_stats)), function(i){
  list(stats_title = v4_stats$stats_title[i], content =v4_stats$content[i])
  
})

test_input_v4_stats <- my_list_v4_stats[1:10]


# v4 timeline
test_v4_timeline_1 <- readRDS("Testv4_timeline_1.RData")

test_v4_timeline_2 <- readRDS("Testv4_timeline_2.RData")

v4_timeline <- list(test_v4_timeline_1, test_v4_timeline_2)

str(v4_timeline)



combined_test_list_with_v4 <- c(combined_test_list, test_input_v4_stats,
                                v4_timeline)

length(combined_test_list_with_v4)

str(combined_test_list_with_v4)

saveRDS(combined_test_list_with_v4, file = "stats_timeline_test_list_with_v4.RData")

test_list_w_v4 <-readRDS("stats_timeline_test_list_with_v4.RData")
length(test_list_w_v4)






#### Creating the actual processing data
stats_data_raw <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/post_game_stats_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

timeline_data_raw <- read_csv("C:/Users/house/Desktop/Getting into Modelling/Scripts/post_game_timeline_raw.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))


stats_list <- lapply(seq_len(nrow(stats_data_raw)), function(i){
  list(stats_title = stats_data_raw$stats_title[i], content =stats_data_raw$content[i])
  
})


timeline_list <- lapply(seq_len(nrow(timeline_data_raw)), function(i){
  list(stats_title = timeline_data_raw$stats_title[i], content =timeline_data_raw$content[i])
  
})

length(stats_list)
length(timeline_list)

combined_list <- c(stats_list, timeline_list)
length(combined_list)
str(combined_list)

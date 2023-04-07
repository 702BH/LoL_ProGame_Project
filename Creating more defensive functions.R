# Creating more defensive functions


# check input data before sending anywhere
function_check_input_valid <- function(list){
  # check current element is a list
  
  if(is.list(list) && all(c("stats_title", "content") %in% names(list))){
    content <- list[["content"]]
    if(is.character(content) && !is.na(content)){
      if(validate(content)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
  
}

check_list_results <- unlist(lapply(combined_test_list, function_check_input_valid))

my_list_filtered <- combined_test_list[check_list_results]


function_check_input_valid_return <- function(list){
  
  check_list_results <- unlist(lapply(list, function_check_input_valid))
  
  valid_lists <- list[check_list_results]
  invalid_lists <- list[!check_list_results]
  
  return(list(valid_lists, invalid_lists))
}

test <- function_check_input_valid_return(combined_test_list)

valid <- test[[1]]
length(valid)
valid

invalid <- test[[2]]
invalid

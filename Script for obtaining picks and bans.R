# Script for obtaining picks and bans
library(tidyverse)
library(httr)
library(jsonlite)


# TEST 1
api_base_url <- "https://lol.fandom.com/api.php"

# probbaly going to have get the picks and bans for each tournennamt again, since max is 500

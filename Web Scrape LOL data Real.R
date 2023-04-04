# Web Scrape LOL data Real

library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)



# Helper Data 3 (function 5 or -3)
spell_data_json <- function(ddspell_url){
  
  spell_json <- fromJSON(ddspell_url)
  spell_data <- spell_json$data
  
  spell_tibble <- tibble(spells = spell_data)
  
  spell_unnest <- spell_tibble %>%
    unnest_wider(spells)
  
  final_spell <- spell_unnest %>%
    select(name, key)
  
  # conver the key to integer
  final_spell$key <- as.integer(final_spell$key)
  
 
  return(final_spell) 
  
}



json_spell <- spell_data_json("http://ddragon.leagueoflegends.com/cdn/12.3.1/data/en_US/summoner.json")


view(json_spell)
class(json_spell$key)


# Helper Data 2 (function 4 or -2)
item_data_json <- function(dditem_url){
  
  item_json <- fromJSON(dditem_url)
  item_data <- item_json$data
  
  item_df <- data.frame(id = character(),
                        name = character())
  
  item_d_length <- length(item_data)
  
  for(i in 1:item_d_length){
    
    id = names(item_data[i])
    name = item_data[[i]][1]
    
    temp_df <- data.frame(id, name)
    
    item_df <- bind_rows(item_df, temp_df)
    
  }
  
  new_df <- c(0, "No Item")
  
  new_df2 <- data.frame(id = c(7000, 7001, 7002, 7003, 7004, 7005, 7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7021,	7022, 7024),
                        name = c("Sandshrike's Claw",	"Syzygy",	"Draktharr's Shadowcarver",	"Turbocharged Hexperiment",	"Forgefire Crest",	"Rimeforged Grasp",
                        "Typhoon",	"Wyrmfallen Sacrifice",	"Bloodward",	"Icathia's Curse",	"Vespertide",	"Upgraded Aeropack",	"Liandry's Lament",	"Eye of Luden",
                        "Eternal Winter",	"Ceaseless Hunger",	"Dreamshatter",	"Deicide",	"Infinity Force", "Reliquary of the Golden Dawn",	"Shurelya's Requiem",	"Starcaster",	"Seat of Command", "Caesura"))
  
  item_df <- rbind(item_df, new_df)
  
  item_df <- rbind(item_df, new_df2)
  
  item_df$id <- as.numeric(item_df$id)
  
  return(item_df)
  
}

# NOTE: these names cant change, as they are used in the below function
json_item <- item_data_json("http://ddragon.leagueoflegends.com/cdn/12.3.1/data/en_US/item.json")

view(json_item)



# Helper Data (function 3 or -1)
champion_data_json <- function(ddchamp_url){
  
  # read in data
  json_data <- fromJSON(ddchamp_url)
  champion_data <- json_data$data
  
  test_data <- tibble(champion = champion_data) %>%
    unnest_wider(champion) %>%
    unnest_wider(info) %>%
    unnest_wider(tags) %>%
    unnest_wider(stats)
  
  col_change <- match(c("...1", "...2"), colnames(test_data))
  colnames(test_data)[col_change] <- c("primary_role", "secondary_role")
  
  # select the desired columns
  test_data <- test_data %>%
    select(key, name, attack, defense, magic, difficulty, primary_role, secondary_role)
  
  test_data$key <- as.integer(test_data$key)
  
  # replace NA with "None"
  test_data$secondary_role <- replace_na(test_data$secondary_role, "None")
  
  # rename name to champion name
  test_data <- test_data %>%
    rename(champion_name = name)
  
  return(test_data)
  
}
# NOTE: these names cant change, as they are used in the below function
champ_data <- champion_data_json("http://ddragon.leagueoflegends.com/cdn/12.3.1/data/en_US/champion.json")

view(champ_data)



## helper function 5
dynamic_mh_func <- function(url){
  
  
  scrape_page <- read_html(url)
  
  
  
  infobox <- scrape_page %>%
    html_nodes(xpath = '//table[@id = "infoboxTournament"]//th[@class = "infobox-title"]') %>%
    html_text2()
  
  split <- str_split(infobox, " ") %>%
    unlist()
  
  leng_split <- length(split)
  
  if(leng_split == 3){
    
    league <- split[1]
    season <- split[3]
    year <- split[2]
    
    if(season == "Championship"){
      
      base1 <- "https://lol.fandom.com/Special:RunQuery/MatchHistoryGame?MHG%5Btournament%5D="
      base2 <- "/"
      base3 <- "%20Season/"
      base4 <- "&MHG%5Blimit%5D=300&MHG%5Bpreload%5D=Tournament&MHG%5Bspl%5D=yes&pfRunQueryFormName=MatchHistoryGame"
      
      final_url <- paste0(base1, league,base2, year,base3, season, base4)
      
      
      
      
      
    } else{
      
      base1 <- "https://lol.fandom.com/Special:RunQuery/MatchHistoryGame?MHG%5Btournament%5D="
      base2 <- "/"
      base3 <- "%20Season/"
      base4 <- "%20Season&MHG%5Blimit%5D=300&MHG%5Bpreload%5D=Tournament&MHG%5Bspl%5D=yes&pfRunQueryFormName=MatchHistoryGame"
      
      final_url <- paste0(base1, league,base2, year,base3, season, base4)
      
      
    }
    
    
  }else{
    
    league <- split[1]
    season <- split[3]
    year <- split[2]
    extra <- split[4]
    
    
    if(season == "Championship"){
      
      base1 <- "https://lol.fandom.com/Special:RunQuery/MatchHistoryGame?MHG%5Btournament%5D="
      base2 <- "/"
      base3 <- "%20Season/"
      base4 <- "&MHG%5Blimit%5D=300&MHG%5Bpreload%5D=Tournament&MHG%5Bspl%5D=yes&pfRunQueryFormName=MatchHistoryGame"
      
      final_url <- paste0(base1, league,base2, year,base3, season, base4)
      
      
      
      
      
    } else{
      
      
      
      
      base1 <- "https://lol.fandom.com/Special:RunQuery/MatchHistoryGame?MHG%5Btournament%5D="
      base2 <- "/"
      base3 <- "%20Season/"
      base4 <- "&MHG%5Blimit%5D=300&MHG%5Bpreload%5D=Tournament&MHG%5Bspl%5D=yes&pfRunQueryFormName=MatchHistoryGame"
      
      final_url <- paste0(base1, league,base2, year,base3, season, "%20", extra, base4)
      
      
    }
    
    

    
  }
  
  return(final_url)
}
  
  




# Function 1
# From overview page, scrape the VODs table and clean it
# also scrape and add the links

scrape_vods_table_func <- function(url){
  
  scrape_page <- read_html(url)
  
  # scrape the table
  scrape_table <- scrape_page %>%
    html_nodes(xpath = '//table[@id = "md-table"]') %>%
    html_table()
  
  # scrape the column names
  col_names <- scrape_page %>%
    html_nodes(xpath = '//table[@id = "md-table"]//tbody//tr[position() = 3]/th') %>%
    html_text()
  
  # create a data frame
  scrape_table <- as.data.frame(scrape_table)
  
  # assign the column names using the scraped names
  colnames(scrape_table) <- col_names
  
  # removing MVP to avoid errors
  scrape_table <- scrape_table %>%
    select(-MVP)
  
  
  # cleaning table, removing redundancy left over from scrape
  final_table <- scrape_table %>%
    filter(!str_detect(`Team 1`, '[showhide]')) %>%
    filter(!str_detect(Blue, 'TBD')) %>%
    filter(MH == "Link")
  
  
  # scraping links and adding them to the table
  # using CSS selectors this time
  body_table <- scrape_page %>%
    html_nodes(xpath = '//table[@id = "md-table"]')
  
  table_links <- html_nodes(body_table, xpath = ".//a[@title and contains(., 'Link')]") %>%
    html_attr('href')
  
  table_links_complete <- paste0("https://lol.fandom.com",table_links)
  
  
  final_table$MH <- table_links_complete
  
  # removing everything before and including :
  # seperating the final component into platform and gameid
   
   temp_split_1 <- str_remove(final_table$MH, ".*:") %>%
     str_split("_", simplify = TRUE)
   
   final_table$platformid <- temp_split_1[,1]
   
   final_table$gameid <- temp_split_1[,2]
   
   # creating the match title to be used in the api
   temp_split_mt <- str_remove(final_table$MH, ".*/") %>%
     str_replace_all("_", " ") %>%
     str_replace("meta", "")
   
   final_table$match_title <- temp_split_mt
   
   

   # adding league, year, split

   # scrape
   infobox <- scrape_page %>%
     html_nodes(xpath = '//table[@id = "infoboxTournament"]//th[@class = "infobox-title"]') %>%
     html_text2()
   infobox

   df <- tibble(infobox)

   # split into three columns
   temp_df <- df %>%
     separate(infobox, c("league", "year", "split"), sep = " ")

   # add to final_table
   final_table$league <- temp_df$league
   final_table$year <- temp_df$year
   final_table$split <- temp_df$split


   # add the Date of the match
   temp_link <- dynamic_mh_func(url)

   temppage_mh <- read_html(temp_link)

   find_date <- temppage_mh %>%
     html_nodes(xpath = '//div[@class = "wide-content-scroll"]//table/tbody/tr/td[position() =1]') %>%
     html_text2()

   date <- as.Date.character(find_date, tryFormats = c("%Y-%m-%d"))

   date <- date[order(date)]

   final_table <- cbind(final_table, date)

   # add the patch of the match

   find_patch <- temppage_mh %>%
     html_nodes(xpath = '//div[@class = "wide-content-scroll"]//table/tbody/tr/td[position() =2]') %>%
     html_text2()


   patch <- find_patch[order(find_patch)]

   final_table <- cbind(final_table, patch)

   

  return(final_table)
  
  
  
}

vods_table_v4 <- scrape_vods_table_func("https://lol.fandom.com/wiki/VCS/2021_Season/Winter_Season")
view(vods_table_v4)

vods_table_v5 <- scrape_vods_table_func("https://lol.fandom.com/wiki/LCK/2022_Season/Spring_Season")
view(vods_table_v5)

# NOTE: these names cant change, as they are used in the below function
vods_sum_table <- vods_table_v5

view(vods_sum_table)



# test
test_vod <- scrape_vods_table_func("https://lol.fandom.com/wiki/LEC/2022_Season/Spring_Season")

view(test_vod)




# Function 2
# take a match tital from an overview table and extract the Json data for players and teams
api_to_json_helper <- function(match_title, vods_sum_table){
  
  api_base_url <- "https://lol.fandom.com/api.php"
  
  query_param <- list(
    action = "query",
    format = "json",
    prop = "revisions",
    titles = match_title,
    rvprop = "content",
    rvslots = "main"
  )
  
  match_api_data <- GET(api_base_url, query = query_param)
  
  api_content <- content(match_api_data)
  
  api_unnest <- api_content %>%
    unnest_longer(query) %>%
    unnest_wider(query) %>%
    unnest_longer(revisions) %>%
    unnest_wider(revisions) %>%
    unnest_wider(slots) %>%
    unnest_wider(main) %>%
    unnest_longer(`*`)
  
  api_final <- fromJSON(api_unnest$`*`)
  
  
  # game duration
  game_length <- api_final$gameDuration
  
  game_length <- game_length / 60
  
  
  
  # Add dragon type information
  
  # get the timeline data
  
  timeline_title <- paste0(match_title, "/Timeline")
  
  query_param_timeline <- list(
    action = "query",
    format = "json",
    prop = "revisions",
    titles = timeline_title,
    rvprop = "content",
    rvslots = "main"
  )
  
  timeline_api_data <- GET(api_base_url, query = query_param_timeline)
  
  api_content_timeline <- content(timeline_api_data)
  
  api_unnest_time <- api_content_timeline %>%
    unnest_longer(query) %>%
    unnest_wider(query) %>%
    unnest_longer(revisions) %>%
    unnest_wider(revisions) %>%
    unnest_wider(slots) %>%
    unnest_wider(main) %>%
    unnest_longer(`*`)
  
  final_timeline <- fromJSON(api_unnest_time$`*`)
  
  # selecting the timeline events
  events <- final_timeline$frames
  
  events2 <- events %>%
    unnest_longer(events)
  
  events3 <- events2$events
  
  
  # Selecting timeline participant information
  # selecting the timestamps, and the participant values at timestamps
  frames <- final_timeline$frames$timestamp
  
  p_frames <- final_timeline$frames$participantFrames
  
  # bind these together
  frames_bind <- cbind(p_frames, frames)
  
  # pivot longer
  frames_pivot <- pivot_longer(frames_bind, 1:10)
  
  # select columns of interest
  p_values <- frames_pivot$value
  
  p_frames_select <- frames_pivot$frames
  
  timestamp_data <- cbind(p_frames_select, p_values)
  
  # prepare data for joining
  temp_filter <- timestamp_data %>%
    filter((p_frames_select > 590000 & p_frames_select < 650000) | (p_frames_select > 895000 & p_frames_select < 930000)) %>%
    select(p_frames_select, participantId, totalGold, xp, minionsKilled, jungleMinionsKilled)
  
  table_names <- temp_filter %>%
    mutate(time = case_when(
      
      p_frames_select > 590000 & p_frames_select < 650000 ~ 10,
      TRUE ~ 15
      
    ),
    teamId = case_when(
      
      participantId <= 5 ~ 100,
      TRUE ~ 200
      
    )) %>%
    select(-p_frames_select)
  
  final_time_data <- pivot_wider(table_names, names_from = time, values_from = c(totalGold, xp, minionsKilled, jungleMinionsKilled))
  
  # calculating team level values
  team_group <- final_time_data %>%
    select(-participantId) %>%
    group_by(teamId) %>%
    summarise_all(sum)
  

  
  # first identify if the match is V4 or V5, which will determine the processing
  if(str_detect(match_title, "V4")){
    
    
    participants <- api_final$participants
    
    participants_stats <- participants$stats
    
    # select the relevent from participants and then bind
    participants_select <- participants %>%
      select(teamId, championId, spell1Id, spell2Id)
    
    bind <- cbind(participants_select, participants_stats)
    
    # identifying variables to remove from pre-defined match_list
    match_list <- c("combatPlayerScore", "objectivePlayerScore","totalPlayerScore", "totalScoreRank","playerScore0", "playerScore1",
                    "playerScore2","playerScore3","playerScore4","playerScore5","playerScore6","playerScore7","playerScore8","playerScore9",
                    "perk0","perk0Var1","perk0Var2","perk0Var3","perk1","perk1Var1","perk1Var2","perk1Var3","perk2","perk2Var1","perk2Var2",
                    "perk2Var3","perk3","perk3Var1","perk3Var2","perk3Var3", "perk4","perk4Var1","perk4Var2","perk4Var3","perk5","perk5Var1",
                    "perk5Var2","perk5Var3","perkPrimaryStyle","perkSubStyle","statPerk0","statPerk1","statPerk2") 
    
    match_cols <- match(match_list, colnames(bind))
    bind_remove <- bind[-match_cols]
    
  
    
    
    # converting logicals to integers
    cols_log <- sapply(bind_remove, is.logical)
    
    bind_remove[,cols_log] <- lapply(bind_remove[,cols_log], as.integer)
    
    
    # selecting data
    aggregate_data <- bind_remove %>%
      select(-c(championId, spell1Id, spell2Id, participantId, win, item0, item1, item2, item3, item4, item5, item6))

    aggregate_sum <- aggregate_data %>%
      group_by(teamId) %>%
      summarise_all(sum)
    
    
    
    # Adding gameid
    gameid <- api_final$gameId
    
    bind_final <- cbind(gameid, bind_remove)
    
    # adding champion
    
    bind_final <- left_join(bind_final, champ_data, by =c("championId" = "key"))
    
    bind_final <- bind_final %>%
      relocate(champion_name, .before = championId)
    
    # adding the player name and team
    
    # extract participant identiy data
    part_id <- api_final$participantIdentities
    
    # unnest df and select
    id_unnest <- do.call(data.frame, part_id)
    
    ids_select <- id_unnest %>%
      select(participantId, player.summonerName)
    
    # split team from player, unest, rename
    ids_select$player.summonerName <- str_split(ids_select$player.summonerName, " ", simplify = TRUE)
    
    unest_id <- do.call(data.frame, ids_select)
    unest_id <- unest_id %>%
      rename(team_name = player.summonerName.1, 
             player_name = player.summonerName.2)
    
    
    # left_join to final table based on participantId
    bind_final <- left_join(bind_final, unest_id, by= "participantId")
    
    bind_final <- bind_final %>%
      relocate(c(team_name, player_name), .before = participantId)
    
    # adding item_names
    
    item_list <- c("item0", "item1", "item2", "item3", "item4", "item5", "item6")
    
    match_c <- which(colnames(bind_final) %in% item_list)
    
    match_c
    
    
    for(i in match_c){
      
      temp <-  sapply(bind_final[,i], function(value) which(json_item$id == value)) %>%
        unlist
      
      bind_final[,i] <- json_item[temp,]$name
      
      
      
    }
    
    
    # adding spell names
    temp_spell_data <- bind_final %>%
      select(player_name, spell1Id, spell2Id)
    
    bind_final <- bind_final %>%
      select(- c(spell1Id, spell2Id))
    
    # pivot longer to 1 column
    spells_pivot <- pivot_longer(temp_spell_data, cols = c("spell1Id", "spell2Id"), names_to = "spells")
    
    # join the names
    spells_join <- left_join(spells_pivot, json_spell, by = c("value" = "key")) %>%
      select(-value)
    
    spells_wide <- pivot_wider(spells_join, names_from = spells, values_from = name)
    
    
    bind_final <- bind_final %>%
      left_join(spells_wide,  by = "player_name") %>%
      relocate(c(spell1Id, spell2Id),  .after = champion_name)
    
    
    
    # adding participant time data
    
    bind_final <- left_join(bind_final, final_time_data, by = "participantId")
    
    
    # manually add team ID
    bind_final <- bind_final %>%
      mutate(teamId = case_when(
        
        participantId <= 5 ~ 100,
        TRUE ~ 200
        
      ))

    
    
    
    
    
    


    # extracting team level information
    teams_api <- api_final$teams

    teams_api_unnest <- teams_api %>%
      unnest_wider(bans) %>%
      unnest_wider(championId) %>%
      select(-pickTurn)

    # renaming bans (because unamed list)
    col_change_teams <- match(c("...1", "...2", "...3", "...4", "...5"), colnames(teams_api_unnest))
    colnames(teams_api_unnest)[col_change_teams] <- c("ban_1", "ban_2", "ban_3", "ban_4", "ban_5")

    # add gameid


    teams_bind_id <- cbind(gameid, teams_api_unnest)

    # change logicals to integers
    cols_team <- sapply(teams_bind_id, is.logical)

    teams_bind_id[,cols_team] <- lapply(teams_bind_id[,cols_team], as.integer)

    # win is in character
    # changing win values
    teams_bind_id$win[teams_bind_id$win == "Win"] <- "TRUE"
    teams_bind_id$win[teams_bind_id$win == "Fail"] <- "FALSE"

    teams_bind_id$win <- as.integer(as.logical(teams_bind_id$win))



    # remove_list
    match_llist_teams <- c("dominionVictoryScore", "vilemawKills")

    match_cols <- match(match_llist_teams, colnames(teams_bind_id))
    teams_final <- teams_bind_id[-match_cols]


    # add team names
    team_100 <- unest_id$team_name[1]
    team_200 <- unest_id$team_name[6]


    teams_final <- teams_final %>%
      mutate(team_name = case_when(
        teamId == 100 ~ team_100,
        teamId == 200 ~ team_200

      ))

    # add dragons
    # filter for dragon kills
    events_filtered <- events3 %>%
      filter(type == "ELITE_MONSTER_KILL" & monsterType == "DRAGON") %>%
      select(killerId, monsterSubType)

    # create teamids
    events_filtered <- events_filtered %>%
      mutate(killerTeamId = case_when(
        killerId <= 5 ~ 100,
        TRUE ~ 200
      ))

    events_filtered <- events_filtered %>%
      select(-killerId)



    # create counts
    kill_counts <- events_filtered %>%
      group_by(killerTeamId) %>%
      count(monsterSubType)

    # pivot the data and replace the NA
    finall_kills <- pivot_wider(kill_counts, names_from = monsterSubType, values_from = n)

    finall_kills <- finall_kills %>%
      replace(is.na(.),0)

    # remove the killer team id
    dragon_timeline <- finall_kills[,-1]



    # add dragon types to team data
    teams_final <- teams_final %>%
      mutate(AIR_DRAGON = 0,
             EARTH_DRAGON = 0,
             FIRE_DRAGON = 0,
             HEXTECH_DRAGON = 0,
             WATER_DRAGON = 0,
             ELDER_DRAGON = 0)


    # getting dragon names in timeline
    cols <- colnames(dragon_timeline)

    # add the values from timeline to team data
    for(name in cols){

      teams_final[name] <- dragon_timeline[name]

    }


    # adding aggregate data
    teams_final <- left_join(teams_final, aggregate_sum, by = "teamId")
    
    
    # add grouped by timeline data
    teams_final <- left_join(teams_final, team_group, by = "teamId")




    # join participant and team data

    joined_data <- bind_rows(bind_final, teams_final)

    # add positions
    joined_data <- joined_data %>%
      mutate(position = case_when(
        participantId == 1 | participantId == 6 ~ "Top",
        participantId == 2 | participantId == 7 ~ "Jng",
        participantId == 3 | participantId == 8 ~ "Mid",
        participantId == 4 | participantId == 9 ~ "Bot",
        participantId == 5 | participantId == 10 ~ "Sup",
        TRUE ~ "Team"

      ))


    # add side
     vods_table_pivot <- vods_sum_table %>%
       pivot_longer(Blue:Red, names_to = "side", values_to = "team")

     vods_table_pivot_select <- vods_table_pivot %>%
       select(gameid, side, team)

     vods_table_pivot_select$gameid <- as.numeric(vods_table_pivot_select$gameid)

     joined_data <- left_join(joined_data, vods_table_pivot_select, by = c("gameid", "team_name" = "team")) %>%
       relocate(side, .after = teamId)


     # add the league, year, split info
     vods_select <- vods_sum_table %>%
       select(gameid, league, year, split)

     vods_select$gameid <- as.numeric(vods_select$gameid)

     joined_data <- left_join(joined_data, vods_select, by = "gameid")

     # add the date
     vods_select_date <- vods_sum_table %>%
       select(gameid, date)

     vods_select_date$gameid <- as.numeric(vods_select_date$gameid)

     joined_data <- left_join(joined_data, vods_select_date, by = "gameid")

     joined_data <- joined_data %>%
       relocate(date, .before = gameid)


     # add the patch
     vods_select_patch <- vods_sum_table %>%
       select(gameid, patch)

     vods_select_patch$gameid <- as.numeric(vods_select_patch$gameid)

     joined_data <- left_join(joined_data, vods_select_patch, by = "gameid")

     joined_data <- joined_data %>%
       relocate(patch, .before = gameid)


     # add game duration
     joined_data <- cbind(joined_data, game_length)

     joined_data <- joined_data %>%
       relocate(game_length, .after = gameid)


    return(joined_data)




  }else{

    # participant data

    part_v5 <- api_final$participants

    v5_gameid <- api_final$gameId

    # remove perks
    part_v5 <- part_v5 %>%
      select(-perks)
    
    # remove challanges if its present
    colnames_part <- colnames(part_v5)
    
    col_match <- sum(match(colnames_part, "challenges", nomatch = 0))
    
    if(col_match > 0){
      
      part_v5 <- part_v5 %>%
        select(-challenges)
      
    }
    
    
    
    # getting data for aggregation for team level
    select_remove <- part_v5 %>%
      select(-c(championId, championName, championTransform, gameEndedInEarlySurrender, gameEndedInSurrender, individualPosition, item0, item1, item2, item3, item4, item5, item6,
                lane, participantId, profileIcon, riotIdName, riotIdTagline, role, spell1Id, spell2Id, summonerId, summonerLevel, summonerName, teamEarlySurrendered, teamPosition,
                win))
    
    aggregate_sum <- select_remove %>%
      group_by(teamId) %>%
      summarise_all(sum)
    
    

    
    

    #add game id
    pert_bind <- cbind(v5_gameid, part_v5)

    # check logicals
    cols_v5 <- sapply(pert_bind, is.logical)

    pert_bind[,cols_v5] <- lapply(pert_bind[,cols_v5], as.integer)


    # team name and summoner name

    to_split <- pert_bind %>%
      select(participantId, summonerName)

    to_split$summonerName <- str_split(to_split$summonerName, " ", simplify = TRUE)

    unnest_split <- do.call(data.frame, to_split)
    unnest_final_split <- unnest_split %>%
      rename(team_name = summonerName.1,
             player_name = summonerName.2)

    # left_join to final table based on teamid
    pert_bind <- left_join(pert_bind, unnest_final_split, by= "participantId")

    pert_bind <- pert_bind %>%
      relocate(c(team_name, player_name), .before = participantId)


    # adding item_names
     item_list <- c("item0", "item1", "item2", "item3", "item4", "item5", "item6")

    match_c <- which(colnames(pert_bind) %in% item_list)

    match_c


    for(i in match_c){

      temp <-  sapply(pert_bind[,i], function(value) which(json_item$id == value)) %>%
        unlist

      pert_bind[,i] <- json_item[temp,]$name



    }

    # adding spell names
    temp_spell_data <- pert_bind %>%
      select(player_name, spell1Id, spell2Id)

    pert_bind <- pert_bind %>%
      select(- c(spell1Id, spell2Id))

    # pivot longer to 1 column
    spells_pivot <- pivot_longer(temp_spell_data, cols = c("spell1Id", "spell2Id"), names_to = "spells")

    # join the names
    spells_join <- left_join(spells_pivot, json_spell, by = c("value" = "key")) %>%
      select(-value)

    spells_wide <- pivot_wider(spells_join, names_from = spells, values_from = name)


    pert_bind <- pert_bind %>%
      left_join(spells_wide,  by = "player_name") %>%
      relocate(c(spell1Id, spell2Id),  .after = championName)


    # adding participant time data

    pert_bind <- left_join(pert_bind, final_time_data, by = "participantId")


    # adding champion data

    pert_bind <- left_join(pert_bind, champ_data, by =c("championId" = "key"))
    
    
    
    # manually add team ID
    pert_bind <- pert_bind %>%
      mutate(teamId = case_when(
        
        participantId <= 5 ~ 100,
        TRUE ~ 200
        
      ))
    



    # team_data
    # select_data
    team_v5_select <- api_final$teams %>%
      select(bans, teamId, win)

    # unnest and rename the bans columns
    team_v5_select_unnest <- team_v5_select %>%
      unnest_wider(bans) %>%
      select(-pickTurn) %>%
      unnest_wider(championId)

    col_change <- match(c("...1", "...2", "...3", "...4", "...5"), colnames(team_v5_select_unnest))
    colnames(team_v5_select_unnest)[col_change] <- c("ban_1", "ban_2", "ban_3", "ban_4", "ban_5")

    # conver the logicals(win)
    cols_team_select <- sapply(team_v5_select_unnest, is.logical)

    team_v5_select_unnest[,cols_team_select] <- lapply(team_v5_select_unnest[,cols_team_select], as.integer)



    # objective_data
    team_obj <- api_final$teams$objectives

    # THIS FIXES THE ISSUE
    unnest_obj <- do.call(data.frame, team_obj)

    # change objectives logical
    cols_team_obj <- sapply(unnest_obj, is.logical)

    unnest_obj[,cols_team_obj] <- lapply(unnest_obj[,cols_team_obj], as.integer)

    # combine the two team data
    bind_team <- cbind(team_v5_select_unnest, unnest_obj)

    # add the gameid

    final_team <- cbind(v5_gameid, bind_team)

    # add team names
    team_100 <- unnest_final_split$team_name[1]
    team_200 <- unnest_final_split$team_name[6]


    final_team <- final_team %>%
      mutate(team_name = case_when(
        teamId == 100 ~ team_100,
        teamId == 200 ~ team_200

      ))


    # add dragons
    # filter for dragon kills
    events_filtered <- events3 %>%
      filter(type == "ELITE_MONSTER_KILL" & monsterType == "DRAGON") %>%
      select(killerTeamId, monsterSubType)

    # create counts
    kill_counts <- events_filtered %>%
      group_by(killerTeamId) %>%
      count(monsterSubType)

    # pivot the data and replace the NA
    finall_kills <- pivot_wider(kill_counts, names_from = monsterSubType, values_from = n)

    finall_kills <- finall_kills %>%
      replace(is.na(.),0)

    # remove the killer team id
    dragon_timeline <- finall_kills[,-1]


    # add dragon types to team data
    final_team <- final_team %>%
      mutate(AIR_DRAGON = 0,
             EARTH_DRAGON = 0,
             FIRE_DRAGON = 0,
             HEXTECH_DRAGON = 0,
             WATER_DRAGON = 0,
             ELDER_DRAGON = 0)


    # getting dragon names in timeline
    cols <- colnames(dragon_timeline)

    # add the values from timeline to team data
    for(name in cols){

      final_team[name] <- dragon_timeline[name]

    }
    
    
    # adding the aggregate data
    final_team <- left_join(final_team, aggregate_sum, by = "teamId")
    
    # add grouped by timeline data
    final_team <- left_join(final_team, team_group, by = "teamId")
    




    # combine the participant and team data

    joined_data_v5 <- bind_rows(pert_bind,final_team)

    joined_data_v5 <- joined_data_v5 %>%
      mutate(position = case_when(
        participantId == 1 | participantId == 6 ~ "Top",
        participantId == 2 | participantId == 7 ~ "Jng",
        participantId == 3 | participantId == 8 ~ "Mid",
        participantId == 4 | participantId == 9 ~ "Bot",
        participantId == 5 | participantId == 10 ~ "Sup",
        TRUE ~ "Team"

      ))


    #add side
    vods_table_pivot <- vods_sum_table %>%
      pivot_longer(Blue:Red, names_to = "side", values_to = "team")

    vods_table_pivot_select <- vods_table_pivot %>%
      select(gameid, side, team)

    vods_table_pivot_select$gameid <- as.numeric(vods_table_pivot_select$gameid)

    joined_data_v5 <- left_join(joined_data_v5, vods_table_pivot_select, by = c("v5_gameid" = "gameid", "team_name" = "team")) %>%
      relocate(side, .after = teamId)


    # add the league, year, split info
    vods_select <- vods_sum_table %>%
      select(gameid, league, year, split)

    vods_select$gameid <- as.numeric(vods_select$gameid)

    joined_data_v5 <- left_join(joined_data_v5, vods_select, by = c("v5_gameid" = "gameid"))



    # add the date
    vods_select_date <- vods_sum_table %>%
      select(gameid, date)

    vods_select_date$gameid <- as.numeric(vods_select_date$gameid)

    joined_data_v5 <- left_join(joined_data_v5, vods_select_date, by = c("v5_gameid" = "gameid"))

    joined_data_v5 <- joined_data_v5 %>%
      relocate(date, .before = v5_gameid)

    # add the patch
    vods_select_patch <- vods_sum_table %>%
      select(gameid, patch)

    vods_select_patch$gameid <- as.numeric(vods_select_patch$gameid)

    joined_data_v5 <- left_join(joined_data_v5, vods_select_patch, by = c("v5_gameid" = "gameid"))

    joined_data_v5 <- joined_data_v5 %>%
      relocate(patch, .before = v5_gameid)



    # add game duration
    joined_data_v5 <- cbind(joined_data_v5, game_length)

    joined_data_v5 <- joined_data_v5 %>%
      relocate(game_length, .after = v5_gameid)


    return(joined_data_v5)
  }



}


vods_sum_table <- vods_table_v4


test_api <- api_to_json_helper("V4 data:ESPORTSTMNT01 2511008", vods_table_v4)

view(test_api)


test_api_v5 <- api_to_json_helper("V5 data:ESPORTSTMNT02 2552210",test_vod)

view(test_api_v5$primary_role)


# broken
# "V5 data:ESPORTSTMNT06 2190198"


for_test_api <- api_to_json_helper("V5 data:ESPORTSTMNT06 2190198", test_vod)

for_test_api$champion.kills

view(for_test_api)




# obtaining the column names that are similar so can select only these
# colnames
v4_cols <- colnames(test_api)

v5_cols <- colnames(test_api_v5)


column_match <- v4_cols[v4_cols %in% v5_cols]


test_df <- data.frame(column_match)

view(test_df)

a <- t(test_df)
view(a)

colnames(a) <- column_match

c <- a[-1,]

view(c)

colnames(c)

# mmatch titles
test_mt <- vods_table_v4$match_title




# using the above functions to scrape the data 
# creating function

# Function 3 Obtain all Match titles

scrape_match_title <- function(list_of_links){
  
  
  return_list <- c()
  
  # scrape vods tables
  for(link in list_of_links){
    
    # current vods table
    temp_vods_table <- scrape_vods_table_func(link)
    
    # current match title
    temp_mt <- temp_vods_table$match_title
    
    return_list <- c(return_list, temp_mt)
    
    
  }
  
  return(return_list)
  
  
}


test_lec_mt <- scrape_match_title(c("https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Season", "https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Playoffs"))

test_lec_mt


scrape_match_data <- function(match_titles, )

  
  



scrape_all_function <- function(list_of_links){
  
  
  # use test data to create an empty data frame with the required column names
  test_df <- data.frame(column_match)
  
  a <- t(test_df)
  
  colnames(a) <- column_match
  
  final_df <- a[-1,]
  
  
  # scrape the vods table
  for(link in list_of_links){
    
    # current vods table
    vods_sum_table <- scrape_vods_table_func(link)
    
    # current MH
    temp_mt <- vods_sum_table$match_title
    
    for(names in temp_mt){
      
      current_temp <- api_to_json_helper(names, vods_sum_table)
      
      current_temp_out <- current_temp %>%
        select(all_of(column_match))
      
      final_df <- rbind(final_df, current_temp_out)
      Sys.sleep(5)
      
      print(link)
      print(names)
      
    }
    
    
    
  }
  
  return(final_df)
  
  
}

lec_spring_2021 <- scrape_all_function(c("https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Season", "https://lol.fandom.com/wiki/LEC/2021_Season/Summer_Season", "https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Playoffs"))



view(lec_spring_2021$position)

all_scrape <- scrape_all_function(c(
  
  "https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Season",
  "https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Playoffs",
  "https://lol.fandom.com/wiki/LEC/2021_Season/Summer_Season",
  "https://lol.fandom.com/wiki/LEC/2021_Season/Summer_Playoffs",
  "https://lol.fandom.com/wiki/LEC/2022_Season/Spring_Season",
  "https://lol.fandom.com/wiki/LCS/2021_Season/Spring_Season",
  "https://lol.fandom.com/wiki/LCS/2021_Season/Mid-Season_Showdown",
  "https://lol.fandom.com/wiki/LCS/2021_Season/Summer_Season",
  "https://lol.fandom.com/wiki/LCS/2022_Season/Spring_Season",
  "https://lol.fandom.com/wiki/LCK/2021_Season/Spring_Season",
  "https://lol.fandom.com/wiki/LCK/2021_Season/Spring_Playoffs",
  "https://lol.fandom.com/wiki/LCK/2021_Season/Summer_Season",
  "https://lol.fandom.com/wiki/LCK/2021_Season/Summer_Playoffs",
  "https://lol.fandom.com/wiki/LCK/2022_Season/Spring_Season"
  
))

view(all_scrape)


# LEC
lec_2021_spring <- scrape_all_function("https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Season")
view(lec_2021_spring)

lec_2021_spring_playoffs <- scrape_all_function("https://lol.fandom.com/wiki/LEC/2021_Season/Spring_Playoffs")
view(lec_2021_spring_playoffs)


lec_2021_summer <- scrape_all_function("https://lol.fandom.com/wiki/LEC/2021_Season/Summer_Season")
view(lec_2021_summer)


lec_2021_summer_playoffs <- scrape_all_function("https://lol.fandom.com/wiki/LEC/2021_Season/Summer_Playoffs")
view(lec_2021_summer_playoffs)


lec_2021_final <- rbind(lec_2021_spring, lec_2021_spring_playoffs, lec_2021_summer, lec_2021_summer_playoffs)
view(lec_2021_final)


write.csv(lec_2021_final, "LEC_2021.csv", row.names = FALSE)




lec_2022_spring <- scrape_all_function("https://lol.fandom.com/wiki/LEC/2022_Season/Spring_Season")
view(lec_2022_spring)



# LCS
lcs_2021_spring <- scrape_all_function("https://lol.fandom.com/wiki/LCS/2021_Season/Spring_Season")
view(lcs_2021_spring)


lcs_2021_mid_season_showdown <- scrape_all_function("https://lol.fandom.com/wiki/LCS/2021_Season/Mid-Season_Showdown")
view(lcs_2021_mid_season_showdown)


lcs_2021_summer <- scrape_all_function("https://lol.fandom.com/wiki/LCS/2021_Season/Summer_Season")
view(lcs_2021_summer)


lcs_2021_championship <- scrape_all_function("https://lol.fandom.com/wiki/LCS/2021_Season/Championship")
view(lcs_2021_championship)


lcs_2021_final <- rbind(lcs_2021_spring, lcs_2021_mid_season_showdown, lcs_2021_summer, lcs_2021_championship)

write.csv(lcs_2021_final, "LCS_2021.csv", row.names = FALSE)



lcs_2022_spring <- scrape_all_function("https://lol.fandom.com/wiki/LCS/2022_Season/Spring_Season")
view(lcs_2022_spring)



# LCK
lck_2021_spring <- scrape_all_function("https://lol.fandom.com/wiki/LCK/2021_Season/Spring_Season")
view(lck_2021_spring)




lck_2021_spring_playoffs <- scrape_all_function("https://lol.fandom.com/wiki/LCK/2021_Season/Spring_Playoffs")
view(lck_2021_spring_playoffs)



lck_2021_summer <- scrape_all_function("https://lol.fandom.com/wiki/LCK/2021_Season/Summer_Season")
view(lck_2021_summer)


lck_2021_summer_playoffs <- scrape_all_function("https://lol.fandom.com/wiki/LCK/2021_Season/Summer_Playoffs")
view(lck_2021_summer_playoffs)

lck_2021_final <- rbind(lck_2021_spring, lck_2021_spring_playoffs, lck_2021_summer, lck_2021_summer_playoffs)

write.csv(lck_2021_final, "LCK_2021.csv", row.names = FALSE)



lck_2022_spring <- scrape_all_function("https://lol.fandom.com/wiki/LCK/2022_Season/Spring_Season")
view(lck_2022_spring)


# all 2021 combined
lol_2021_data <- rbind(lec_2021_final, lcs_2021_final, lck_2021_final)
dim(lol_2021_data)

write.csv(lol_2021_data, "LOL 2021.csv", row.names = FALSE)


# all 2022 combined
lol_2022_data <- rbind(lec_2022_spring, lcs_2022_spring, lck_2022_spring)
write.csv(lol_2022_data, "LOL 2022.csv", row.names = FALSE)

dim(lol_2022_data)







## scraping pick ban orders
scrape_pb_function <- function(league, split, year){
  
  part1 <- "https://lol.fandom.com/wiki/Special:RunQuery/PickBanHistory?PBH%5Bpage%5D="
  part2 <- "&PBH%5Btextonly%5D=Yes&pfRunQueryFormName=PickBanHistory"
  
  url <- paste0(part1, league, "+", year, "+", split, part2)
  
  temp_page <- read_html(url)
  
  table <- temp_page %>%
    html_nodes(xpath = '//table') %>%
    html_table()
  
  table <- data.frame(table)
  names <- table[1,]
  colnames(table) <- names
  table <- table[-1,]
  
  table_final <- table %>%
    separate("RP1-2", c("RP1", "RP2"), sep = ",") %>%
    separate("BP2-3", c("BP2", "BP3"), sep = ",") %>%
    separate("BP4-5", c("BP4", "BP5"), sep = ",")
  
  return(table_final)
  
}

lec_2022 <- scrape_pb_function("LEC", "Spring", "2022")

lcs_2022 <- scrape_pb_function("LCS", "Spring", "2022")

lck_2022 <- scrape_pb_function("LCK", "Spring", "2022")

lpl_2022 <- scrape_pb_function("LPL", "Spring", "2022")

all_pb <- bind_rows(lec_2022, lcs_2022, lck_2022, lpl_2022)

write.csv(all_pb, "LOL 2022 PB.csv", row.names = FALSE)


view(all_pb)

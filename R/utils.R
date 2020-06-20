####Read in Libraries####
library(data.table)
library(tidyverse)
library(Hmisc)
library(RJSONIO)
library(httr)
library(lubridate)


#################
####Functions####
#################

####GET ALL TEAM INFO####
get_all_team_info <- function(season){
  team_info_url <- "http://stats.nba.com/stats/commonTeamYears/?LeagueID=00"
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  res <-
    httr::GET(team_info_url,
              httr::add_headers(.headers = headers))
  team_info_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  team_info <- data.frame(do.call(rbind, team_info_json$resultSets[[1]]$rowSet))
  names(team_info) <- team_info_json$resultSets[[1]]$headers
  team_info <-  
    team_info %>%
    mutate_if(is.list, unlist)
  team_info <- 
    team_info %>%
    filter(is.na(ABBREVIATION) == FALSE) %>%
    select(-c(LEAGUE_ID, MIN_YEAR, MAX_YEAR)) %>% 
    arrange(ABBREVIATION)
  return(team_info)
}


####GET TEAM INFO BY SEASON####
get_team_info_by_season <- function(season){
  player_url <- paste0("http://stats.nba.com/stats/commonallplayers/?LeagueID=00&Season=", season, "&IsOnlyCurrentSeason=1")
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  res <-
    httr::GET(player_url,
              httr::add_headers(.headers = headers))
  player_info_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  player_info_dat <- data.frame(do.call(rbind, player_info_json$resultSets[[1]]$rowSet))
  if(nrow(player_info_dat) == 0){
    col_names <- player_info_json$resultSets[[1]]$headers
    player_info_dat <- data.frame(matrix(ncol = length(col_names), nrow = 0))
    names(player_info_dat) <- col_names
    return(player_info_dat)
    break
  }
  names(player_info_dat) <- player_info_json$resultSets[[1]]$headers
  player_info_dat <-  
    player_info_dat %>%
    mutate_if(is.list, unlist) %>%
    filter(TEAM_ID != 0) %>%
    select(TEAM_ID, TEAM_ABBREVIATION) %>%
    unique() %>%
    arrange(TEAM_ABBREVIATION) %>%
    mutate(season = season)
  return(player_info_dat)
}


####FULL TEAM INDEX####
full_team_index <- function(){
  years = 1946:year(Sys.Date())
  team_list <-list()
  for(i in seq_along(years)){
    team_list[[i]] <- get_team_info_by_season(season = years[i])
    Sys.sleep(1)
  }
  team_df <- do.call(rbind, team_list)
  full_team_df <-
    team_df %>%
    group_by(TEAM_ID, TEAM_ABBREVIATION) %>%
    dplyr::summarize(season = max(season)) %>%
    ungroup() %>%
    distinct() %>%
    group_by(TEAM_ID) %>%
    arrange(TEAM_ID, -season) %>%
    mutate(Team_Num = sprintf("%02d", 1:n()), var_type = "TEAM_ABBREVIATION") %>%
    unite(combi, var_type, Team_Num) %>%
    select(-c(season)) %>%
    spread(combi, TEAM_ABBREVIATION)
  return(full_team_df)
}


####GET TEAM LINEUPS####
get_rosters <- function(game_id, team_id){
  base_url <- "http://stats.nba.com/stats/boxscoreplayertrackv2/?"
  rosters_url <- paste0(base_url, "GameID=", game_id)
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  res <-
    httr::GET(rosters_url,
              httr::add_headers(.headers = headers))
  roster_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  roster_df <- data.frame(do.call(rbind, roster_json$resultSets[[1]]$rowSet))
  names(roster_df) <- roster_json$resultSets[[1]]$headers
  roster_df <-  
    roster_df %>%
    mutate_if(is.list, unlist) %>%
    filter(TEAM_ID == team_id, 
           MIN != "0:00") %>%
    select(c(GAME_ID, TEAM_ID, TEAM_ABBREVIATION, PLAYER_ID, PLAYER_NAME)) %>%
    distinct()
  return(roster_df)
}



####GET STARTERS####
get_starters <- function(game_id, team_id){
  base_url <- "http://stats.nba.com/stats/boxscoreplayertrackv2/?"
  starters_url <- paste0(base_url, "GameID=", game_id)
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  res <-
    httr::GET(starters_url,
              httr::add_headers(.headers = headers))
  starter_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  starter_df <- data.frame(do.call(rbind, starter_json$resultSets[[1]]$rowSet))
  names(starter_df) <- starter_json$resultSets[[1]]$headers
  starter_df <-  
    starter_df %>%
    mutate_if(is.list, unlist) %>%
    filter(TEAM_ID == team_id, 
           MIN != "0:00") %>%
    select(c(GAME_ID, TEAM_ID, TEAM_ABBREVIATION, PLAYER_ID, PLAYER_NAME, START_POSITION)) %>%
    distinct() %>%
    filter(START_POSITION != "") %>%
    select(-c(START_POSITION))
  return(starter_df)
}


####GET JSON####
get_json <- function(json_url){
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  res <-
    httr::GET(json_url,
              httr::add_headers(.headers = headers))
  pbp_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  return(pbp_json)
}


####ORGANIZE PLAYS####
organize_plays <- function(pbp_json){
  game_id <- pbp_json$parameters$GameID
  if("full_team_df.csv" %in% list.files("data")){
    full_team_df <- fread("data/full_team_df.csv", sep = ",", header = TRUE)
  }else{
    full_team_df <- full_team_index()
  }
  pbp_data <- data.frame(do.call(rbind, pbp_json$resultSets[[1]]$rowSet))
  if(nrow(pbp_data) == 0){
    return(NA)
    break
  }
  names(pbp_data) <- pbp_json$resultSets[[1]]$headers
  pbp_data <-
    pbp_data %>%
    mutate_if(is.list, unlist) %>%
    mutate(HomeSub = as.numeric(grepl("SUB:", pbp_data$HOMEDESCRIPTION)),
           AwaySub = as.numeric(grepl("SUB:", pbp_data$VISITORDESCRIPTION)))
  home_team_id <- pbp_data$PLAYER1_TEAM_ID[which(pbp_data$HomeSub == 1)[1]]
  away_team_id <- pbp_data$PLAYER1_TEAM_ID[which(pbp_data$AwaySub == 1)[1]]
  # season <- 
  #   game_id %>%
  #   substr(4, 5)
  # if(substr(season, 1, 1) == "9"){
  #   season <- as.numeric(paste0("19", season))
  # }else{
  #   season <- as.numeric(paste0("20", season))
  # }
  home_roster <- get_rosters(game_id, team_id = home_team_id)
  away_roster <- get_rosters(game_id, team_id = away_team_id)
  home_starters <- get_starters(game_id, team_id = home_team_id)
  away_starters <- get_starters(game_id, team_id = away_team_id)
  
  ##Home Starters
  if(sum(pbp_data$HomeSub) > 0){
    pbp_data_home_team <- 
      pbp_data %>%
      filter(PLAYER1_TEAM_ID == home_team_id)
    subs_home <- 
      pbp_data %>%
      filter(EVENTMSGTYPE == 8,
             PLAYER1_TEAM_ID == home_team_id) %>%
      select(c(IN = PLAYER2_ID, 
               OUT = PLAYER1_ID,
               IN_NAME = PLAYER2_NAME, 
               OUT_NAME = PLAYER1_NAME))
    
    for(player in all_home_players$player_name){
      if(player %in% subs_1_home$IN & player %in% subs_1_home$OUT){
        if(which(subs_1_home$IN == player)[1] < which(subs_1_home$OUT == player)[1]){
          all_home_players <- all_home_players[-which(all_home_players$player_name == player), ]
        }
      }else if(player %in% subs_1_home$IN & !(player %in% subs_1_home$OUT)){
        all_home_players <- all_home_players[-which(all_home_players$player_name == player), ]
      }
    }
  }
  if(nrow(all_home_players) == 5){
    all_home_players[] <- lapply(all_home_players, as.character)
    all_home_players$player_id <- as.numeric(all_home_players$player_id)
    home_starters <- dplyr::arrange(all_home_players, player_name)
  }
  
  ##Away Starters
  if(sum(pbp_data$AwaySub) > 0){
    pbp_data_away_team <- filter(pbp_data, PLAYER1_TEAM_ABBREVIATION == away_team)
    all_away_players <- data.frame(player_name = unique(pbp_data_away_team$PLAYER1_NAME), player_id = unique(pbp_data_away_team$PLAYER1_ID))
    pbp_data_away_subs <- filter(pbp_data, EVENTMSGTYPE == 8, PLAYER1_TEAM_ABBREVIATION == away_team)
    subs_1_away <- data.frame(IN = pbp_data_away_subs$PLAYER2_NAME, OUT = pbp_data_away_subs$PLAYER1_NAME)
    
    for(player in all_away_players$player_name){
      if(player %in% subs_1_away$IN & player %in% subs_1_away$OUT){
        if(which(subs_1_away$IN == player)[1] < which(subs_1_away$OUT == player)[1]){
          all_away_players <- all_away_players[-which(all_away_players$player_name == player), ]
        }
      }else if(player %in% subs_1_away$IN & !(player %in% subs_1_away$OUT)){
        all_away_players <- all_away_players[-which(all_away_players$player_name == player), ]
      }
    }
  }
  if(nrow(all_away_players) == 5){
    all_away_players[] <- lapply(all_away_players, as.character)
    all_away_players$player_id <- as.numeric(all_away_players$player_id)
    away_starters <- dplyr::arrange(all_away_players, player_name)
  }
  
  ##Home Lineups - Subs
  home_lineup_names <- home_starters$player_name
  home_lineups_ids <- home_starters$player_id
  lineups_home <- list()
  lineups_home_ids <- list()
  lineups_home[[1]] <- home_starters$player_name
  lineups_home_ids[[1]] <- home_starters$player_id
  for(plays in 2:nrow(pbp_data)){
    if(pbp_data$HomeSub[plays] == 0){
      lineups_home[[plays]] <- lineups_home[[plays-1]]
      lineups_home_ids[[plays]] <- lineups_home_ids[[plays-1]]
    }else{
      home_lineup_names <- replace(home_lineup_names, home_lineup_names==pbp_data$PLAYER1_NAME[plays], pbp_data$PLAYER2_NAME[plays])
      lineups_home[[plays]] <- home_lineup_names
      home_lineups_ids <- replace(home_lineups_ids, home_lineups_ids==pbp_data$PLAYER1_ID[plays], pbp_data$PLAYER2_ID[plays])
      lineups_home_ids[[plays]] <- home_lineups_ids
    }
  }
  lineups_home_df <- data.frame(do.call(rbind, lineups_home))
  lineups_home_ids_df <- data.frame(do.call(rbind, lineups_home_ids))
  names(lineups_home_df) <- paste0("Home_Player_", c(1:5))
  names(lineups_home_ids_df) <- paste0("Home_Player_", c(1:5))
  
  
  ##Away Lineups - Subs
  away_lineup_names <- away_starters$player_name
  away_lineups_ids <- away_starters$player_id
  lineups_away <- list()
  lineups_away_ids <- list()
  lineups_away[[1]] <- away_starters$player_name
  lineups_away_ids[[1]] <- away_starters$player_id
  for(plays in 2:nrow(pbp_data)){
    if(pbp_data$AwaySub[plays] == 0){
      lineups_away[[plays]] <- lineups_away[[plays-1]]
      lineups_away_ids[[plays]] <- lineups_away_ids[[plays-1]]
    }else{
      away_lineup_names <- replace(away_lineup_names, away_lineup_names==pbp_data$PLAYER1_NAME[plays], pbp_data$PLAYER2_NAME[plays])
      lineups_away[[plays]] <- away_lineup_names
      away_lineups_ids <- replace(away_lineups_ids, away_lineups_ids==pbp_data$PLAYER1_ID[plays], pbp_data$PLAYER2_ID[plays])
      lineups_away_ids[[plays]] <- away_lineups_ids
    }
  }
  lineups_away_df <- data.frame(do.call(rbind, lineups_away))
  lineups_away_ids_df <- data.frame(do.call(rbind, lineups_away_ids))
  names(lineups_away_df) <- paste0("Away_Player_", c(1:5))
  names(lineups_away_ids_df) <- paste0("Away_Player_", c(1:5))
  
  ##Add Columns of Lineups
  pbp_data$Home_Lineup <- sapply(1:length(lineups_home), function(x) paste0(lineups_home[[x]], collapse = " - "))
  pbp_data$Home_Lineup_IDs <- sapply(1:nrow(lineups_home_ids_df), function(x) paste0(lineups_home_ids_df[x,], collapse = " - "))
  pbp_data$Away_Lineup <- sapply(1:length(lineups_away), function(x) paste0(lineups_away[[x]], collapse = " - "))
  pbp_data$Away_Lineup_IDs <- sapply(1:nrow(lineups_away_ids_df), function(x) paste0(lineups_away_ids_df[x,], collapse = " - "))
  pbp_data$Full_Lineups <- sapply(1:nrow(pbp_data), function(x) paste(pbp_data$Home_Lineup[x], pbp_data$Away_Lineup[x], sep = " - "))
  pbp_data$Full_Lineups_IDs <- sapply(1:nrow(pbp_data), function(x) paste(pbp_data$Home_Lineup_IDs[x], pbp_data$Away_Lineup_IDs[x], sep = " - "))
  pbp_data <- cbind(pbp_data, lineups_home_df)
  pbp_data <- cbind(pbp_data, lineups_home_ids_df)
  pbp_data <- cbind(pbp_data, lineups_away_df)
  pbp_data <- cbind(pbp_data, lineups_away_ids_df)
  game_list[[quarter]] <- pbp_data
  full_game_pbp <- do.call(rbind, game_list)
}






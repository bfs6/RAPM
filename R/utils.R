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


####GET TEAM COMMON INFO####
get_team_common_info <- function(season, team_id){
  team_url <- paste0("http://stats.nba.com/stats/teaminfocommon/?LeagueID=00&Season=", season, "&TeamID=", team_id)
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
    httr::GET(team_url,
              httr::add_headers(.headers = headers))
  team_info_json <-
    res$content %>%
    rawToChar() %>%
    RJSONIO::fromJSON(simplifyVector = TRUE, nullValue = NA)
  team_info_dat <- data.frame(do.call(rbind, team_info_json$resultSets[[1]]$rowSet))
  if(nrow(team_info_dat) == 0){
    col_names <- team_info_json$resultSets[[1]]$headers
    team_info_dat <- data.frame(matrix(ncol = length(col_names), nrow = 0))
    names(team_info_dat) <- col_names
    return(team_info_dat)
    next
  }
  names(team_info_dat) <- team_info_json$resultSets[[1]]$headers
  team_info_dat <-  
    team_info_dat %>%
    mutate_if(is.list, unlist)
  Sys.sleep(0.1)
  return(team_info_dat)
}


####FULL TEAM INDEX####
full_team_index <- function(){
  years = 1946:year(Sys.Date())
  team_info <- get_all_team_info()
  team_ids <- team_info$TEAM_ID
  season_and_teams <- expand.grid(season = years, team_id = team_ids)
  full_team_df <- map_df(season_and_teams[1:10,], get_team_common_info)
  return(full_team_df)
}


####GET TEAM LINEUPS####
get_team_lineups <- function(game_id, team_id){
  base_url <- "http://stats.nba.com/stats/commonTeamYears/"
  team_lineup_url <- paste0(base_url, "GameID=", game_id, "&TeamID=", team_id)
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
  team_info <- get_team_info()
  game_id <- pbp_json$parameters
  pbp_data <- data.frame(do.call(rbind, pbp_json$resultSets[[1]]$rowSet))
  if(nrow(pbp_data) == 0){
    return(NA)
    next
  }
  names(pbp_data) <- pbp_json$resultSets[[1]]$headers
  pbp_data <-
    pbp_data %>%
    mutate_if(is.list, unlist) %>%
    mutate(HomeSub = as.numeric(grepl("SUB:", pbp_data$HOMEDESCRIPTION)),
           AwaySub = as.numeric(grepl("SUB:", pbp_data$VISITORDESCRIPTION)))
  home_team_id<- pbp_data$PLAYER1_TEAM_ID[which(pbp_data$HomeSub == 1)[1]]
  away_team_id <- pbp_data$PLAYER1_TEAM_ID[which(pbp_data$AwaySub == 1)[1]]
  
  ##Home Starters
  if(sum(pbp_data$HomeSub) > 0){
    pbp_data_home_team <- 
      pbp_data %>%
      filter(PLAYER1_TEAM_ABBREVIATION == home_team)
    all_home_players <- data.frame(player_name = unique(pbp_data_home_team$PLAYER1_NAME),
                                   player_id = unique(pbp_data_home_team$PLAYER1_ID))
    pbp_data_home_subs <- filter(pbp_data, EVENTMSGTYPE == 8, PLAYER1_TEAM_ABBREVIATION == home_team)
    subs_1_home <- data.frame(IN = pbp_data_home_subs$PLAYER2_NAME, OUT = pbp_data_home_subs$PLAYER1_NAME)
    
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






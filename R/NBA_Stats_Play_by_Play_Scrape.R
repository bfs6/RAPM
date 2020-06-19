####Read in Libraries####
library(RJSONIO)
library(dplyr)
library(plyr)
library(RCurl)
library(Hmisc)
library(data.table)
library(tidyr)
library(tidyverse)
library(httr)
library(RSelenium)


####Load Helper Functions####
source("R/utils.R")


####Get URLs for 1998 to Present####
##Create Vector of URLs
current_year <- year(Sys.Date())
year_vec <- c(1996:current_year)
year_vec_substr <- substr(year_vec, 3, 4)
game_id_vec <- c(1:1230)
game_id_char_vec <- sprintf("%05d", game_id_vec)
game_id_df <- expand.grid(game_id = game_id_char_vec, year = year_vec_substr)
game_id_df$full_game_id <- paste0("002", game_id_df$year, game_id_df$game_id)
pbp_base_url <- "http://stats.nba.com/stats/playbyplayv2/?GameID="
pbp_urls <- paste0(pbp_base_url, game_id_df$full_game_id, "&StartPeriod=", 1, "&EndPeriod=", 12)


####Scrape from PBP URL List####
##Issue w/ #593
pbp_data_list <- list()
for(url in seq_len(pbp_urls)){
  print(url)
  pbp_json <- fromJSON(pbp_urls[url], nullValue = NA)
  pbp_data <- data.frame(do.call(rbind, pbp_json$resultSets[[1]]$rowSet))
  if(nrow(pbp_data) == 0){
    return(NA)
    next
  }
  names(pbp_data) <- pbp_json$resultSets[[1]]$headers
  #pbp_data[] <- lapply(pbp_data, unlist)
  pbp_data$HomeSub <- 0
  pbp_data$AwaySub <- 0
  pbp_data <-
    pbp_data %>%
    mutate(HomeSub = )
  pbp_data$HomeSub[which(data.frame(do.call(rbind, strsplit(pbp_data$HOMEDESCRIPTION, ":")))[,1] == "SUB")] = 1
  pbp_data$AwaySub[which(data.frame(do.call(rbind, strsplit(pbp_data$VISITORDESCRIPTION, ":")))[,1] == "SUB")] = 1
  home_team <- pbp_data$PLAYER1_TEAM_ABBREVIATION[which(pbp_data$HomeSub == 1)[1]]
  away_team <- pbp_data$PLAYER1_TEAM_ABBREVIATION[which(pbp_data$AwaySub == 1)[1]]
  
  ##Home Starters
  if(sum(pbp_data$HomeSub) > 0){
    pbp_data_home_team <- filter(pbp_data, PLAYER1_TEAM_ABBREVIATION == home_team)
    all_home_players <- data.frame(player_name = unique(pbp_data_home_team$PLAYER1_NAME), player_id = unique(pbp_data_home_team$PLAYER1_ID))
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
  pbp_data_list[[url]] <- full_game_pbp
}
pbp_data_list_backup <- pbp_data_list
names(pbp_data_list) <- sapply(seq_along(pbp_data_list), function(x) strsplit(strsplit(pbp_urls[x], "=")[[1]][2], "&")[[1]][1])


####Change Column Names of Data Frames####
for(i in seq_along(pbp_data_list)){
  print(i)
  names(pbp_data_list[[i]])[which(duplicated(names(pbp_data_list[[i]])) == T)] <- paste0(names(pbp_data_list[[i]])[which(duplicated(names(pbp_data_list[[i]])) == T)], "_ID")
}
pbp_data_list_backup <- pbp_data_list


####Get Rid of NA's####
pbp_data_list <- pbp_data_list[-which(lapply(pbp_data_list, length) == 1)]

##Create DF w/ Names of All Game IDs, Year, and Teams 
games_info <- data.frame(game_ids = names(pbp_data_list),
                         season = rep(NA, length(pbp_data_list)), 
                         home_team = rep(NA, length(pbp_data_list)),
                         home_team_id = rep(NA, length(pbp_data_list)),
                         away_team = rep(NA, length(pbp_data_list)),
                         away_team_id = rep(NA, length(pbp_data_list)))
for(i in seq_along(pbp_data_list)){
  print(i)
  year <- substr(games_info$game_ids[i], 4, 5)
  if(as.numeric(substr(year, 1, 1)) == 9){
    games_info$season[i] <- as.numeric(paste0("19", year))
  }else{
    games_info$season[i] <- as.numeric(paste0("20", year))
  }
  home_team <- pbp_data_list[[i]]$PLAYER1_TEAM_ABBREVIATION[which(pbp_data_list[[i]]$HomeSub == 1)[1]]
  away_team <- pbp_data_list[[i]]$PLAYER1_TEAM_ABBREVIATION[which(pbp_data_list[[i]]$AwaySub == 1)[1]]
  home_team_id <- pbp_data_list[[i]]$PLAYER1_TEAM_ID[which(pbp_data_list[[i]]$HomeSub == 1)[1]]
  away_team_id <- pbp_data_list[[i]]$PLAYER1_TEAM_ID[which(pbp_data_list[[i]]$AwaySub == 1)[1]]
  games_info$home_team[i] <- home_team
  games_info$away_team[i] <- away_team
  games_info$home_team_id[i] <- home_team_id
  games_info$away_team_id[i] <- away_team_id
}

##Create DF w/ Team IDs and Names
team_id_info <- unique(games_info[,c("home_team", "home_team_id")])
names(team_id_info) <- c("team", "team_id")
row.names(team_id_info) <- NULL
team_id_info$team2 <- ifelse(duplicated(team_id_info$team_id) == T, 2, 1)
team_id_info1 <- subset(filter(team_id_info, team2 == 1), select = -c(team2))
team_id_info2 <- subset(filter(team_id_info, team2 == 2), select = -c(team2))
team_id_info <- merge(team_id_info1, team_id_info2, by = c("team_id"), all.x = T)
names(team_id_info) <- c("team_id", "team1", "team2")



####Add to Database####
##Start DB Connection
mydb = dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "nba_pbp_data_scrape", host = "35.199.55.188")

##Write Game ID and Team ID Reference Tables
dbWriteTable(mydb, "game_id_info", games_info, overwrite = T)
dbWriteTable(mydb, "team_id_info", team_id_info, overwrite = T)

##Play by Play Data
for(i in seq_along(pbp_data_list)){
  print(i)
  dbWriteTable(mydb, paste0("play_by_play_game_id_", names(pbp_data_list)[i]), pbp_data_list[[i]], overwrite = T)
}

##Close DB Connection
dbDisconnect(mydb)
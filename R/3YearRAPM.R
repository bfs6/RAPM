####Read in Libraries####
library(RODBC)
library(data.table)
library(randomForest)
library(RColorBrewer)
library(weights)
library(ranger)
library(dplyr)
library(Hmisc)
library(parallel)
library(snow)
library(doSNOW)
library(glmnet)
library(tidyr)
library(SafeBayes)



####Helper Functions####
remove_zero_var_cols <- function(df){
  colsToRemove <- c()
  for(i in 1:ncol(df)){
    if(length(unique(df[,i])) == 1){
      colsToRemove <- c(i, colsToRemove)
    }
  }
  if(length(colsToRemove) >= 1){
    df <- df[,-colsToRemove]
  }
  return(df)
}
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}



####Read in Data####
mydb <- odbcDriverConnect(connection = "Driver={SQL Server Native Client 11.0};server=PHX-SQL07\\BBOPS; database=BOps_DW;trusted_connection=yes;")
nba_lineup_game_dim <- sqlQuery(mydb, "select * from dbo.nba_lineup_game_dim
                                      		where 1 = 1
                                      		and league_id = 00
                                      		and season_type in ('Regular Season', 'Playoffs')")
nba_all_player_stats <- sqlQuery(mydb, "select * from dbo.nba_all_player_stats
                                      		where 1 = 1
                                      		and league_id = 00
                                      		and season_type = 'Regular Season'
                    											and game_id = 'All'
                    											and stat_type = 'Totals'
                                          and team_id <> 'TOT'")
odbcClose(mydb)



####Clean Data####
nba_lineup_game_dim$season_num <- as.numeric(gsub("NBA_", "", nba_lineup_game_dim$season_name))
nba_lineup_game_dim <- remove_zero_var_cols(nba_lineup_game_dim)
nba_all_player_stats <- remove_zero_var_cols(nba_all_player_stats)
nba_all_player_stats_totals <- setDT(nba_all_player_stats)[, lapply(.SD, sum), by = c("person_id", "season_name"), .SDcols = c(names(nba_all_player_stats)[which(grepl("poss_", names(nba_all_player_stats)) == T)], "mp")]



####Stint Evaluation####
seasons_loop_min = min(nba_lineup_game_dim$season_num)
seasons_loop_max = max(nba_lineup_game_dim$season_num)
seasons_loop = c(seasons_loop_min:seasons_loop_max)
for(ssn in seasons_loop){
  ##Filter Down Data for Each Year
  dataRAPM <- filter(nba_lineup_game_dim, season_num %in% c((ssn-2):ssn) & home_away == "Home")
  dataRAPM <- arrange(dataRAPM, game_id, period, -game_clock_int_start)
  # dataRAPM$poss_tot[which(dataRAPM$pts > 0 & dataRAPM$poss_tot == 0)] = rep(1, length(which(dataRAPM$pts > 0 & dataRAPM$poss_tot == 0)))
  #dataRAPM <- filter(dataRAPM, poss_tot > 0 & poss_off > 0 & poss_def > 0)
  
  ##Filter Out Garbage Time Stints
  garbage_time_ind1 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 72000 & dataRAPM$game_clock_int_start > 54000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 25 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 25)
  garbage_time_ind2 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 54000 & dataRAPM$game_clock_int_start > 36000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 20 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 20)
  garbage_time_ind3 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 36000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 10 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 10)
  garbage_time_ind <- unique(c(garbage_time_ind1, garbage_time_ind2, garbage_time_ind3))
  dataRAPM <- dataRAPM[-garbage_time_ind, ]
  dataRAPMPossOffInd <- which(dataRAPM$poss_off == 0)
  dataRAPMPossDefInd <- which(dataRAPM$poss_def == 0)
  
  ##Find All Unique Player IDs
  player_id_refs <- data.frame(person_id = as.vector(t(dataRAPM[,names(dataRAPM)[which(grepl("_person_id",names(dataRAPM)))]])),
                               full_name = as.vector(t(dataRAPM[,names(dataRAPM)[which(grepl("team_player.*_full_name", names(dataRAPM)))]])))
  player_id_refs <- unique(player_id_refs)
  row.names(player_id_refs) = NULL
  unique_ID = unique(player_id_refs$person_id)
  keep_id_unique = match(unique_ID, player_id_refs$person_id)
  player_id_refs = player_id_refs[keep_id_unique,]
  player_id_refs = arrange(player_id_refs, person_id)
  
  ##Find Low Minute Players
  playerTotals <- filter(nba_all_player_stats_totals, season_name == paste0("NBA_", ssn))
  # minCutoff <- summary(playerTotals$mp)[2]
  minCutoff <- 250
  HighMinPlayerTotals <- filter(playerTotals, mp >= minCutoff)
  LowMinPlayerTotals <- filter(playerTotals, mp < minCutoff)
  
  ##Build Stint Matrix
  stintMat <- matrix(0, ncol = nrow(player_id_refs) * 2, nrow = nrow(dataRAPM))
  stintMat2 <- matrix(0, ncol = nrow(player_id_refs) * 2, nrow = nrow(dataRAPM))
  updatedPlayerMinutes <- data.frame(person_id = player_id_refs$person_id, full_name = player_id_refs$full_name, mp = NA, poss_off = NA, poss_def = NA)
  colnames(stintMat) <- c(paste0("Off_", paste0("ID", player_id_refs$person_id)), paste0("Def_", paste0("ID", player_id_refs$person_id)))
  colnames(stintMat2) <- colnames(stintMat)
  for(i in 1:nrow(player_id_refs)){
    stintMat[,i] <- stintMat[,i] + as.numeric(rowSums(dataRAPM[,c(paste0("team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i]))
    stintMat[,i + nrow(player_id_refs)] <- stintMat[,i + nrow(player_id_refs)] - as.numeric(rowSums(dataRAPM[,c(paste0("opp_team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i]))
    
    stintMat2[,i] <- stintMat2[,i] + as.numeric(rowSums(dataRAPM[,c(paste0("opp_team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i]))
    stintMat2[,i + nrow(player_id_refs)] <- stintMat2[,i + nrow(player_id_refs)] - as.numeric(rowSums(dataRAPM[,c(paste0("team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i]))
    
    mp_update <- sum(dataRAPM$mp[which(as.numeric(rowSums(dataRAPM[,c(paste0("team_player", c(1:5), "_person_id"), paste0("opp_team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i])) == 1)])
    poss_off_update <-  sum(dataRAPM$poss_off[which(as.numeric(rowSums(dataRAPM[,c(paste0("team_player", c(1:5), "_person_id"), paste0("opp_team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i])) == 1)])
    poss_def_update <- sum(dataRAPM$poss_def[which(as.numeric(rowSums(dataRAPM[,c(paste0("team_player", c(1:5), "_person_id"), paste0("opp_team_player", c(1:5), "_person_id"))] == player_id_refs$person_id[i])) == 1)])
    updatedPlayerMinutes$mp[which(updatedPlayerMinutes$person_id == player_id_refs$person_id[i])] = mp_update
    updatedPlayerMinutes$poss_off[which(updatedPlayerMinutes$person_id == player_id_refs$person_id[i])] = poss_off_update
    updatedPlayerMinutes$poss_def[which(updatedPlayerMinutes$person_id == player_id_refs$person_id[i])] = poss_def_update
  }
  updatedPlayerMinutes$poss_tot <- updatedPlayerMinutes$poss_off + updatedPlayerMinutes$poss_def
  stintMatFull <- rbind(stintMat, stintMat2)
  stintMatFull <- cbind(1, stintMatFull)
  colnames(stintMatFull)[1] <- "Constant"
  adjMat <- t(stintMatFull) %*% stintMatFull
  margin <- 100 * (dataRAPM$pts)/dataRAPM$poss_off
  margin2 <- 100 * (dataRAPM$opp_pts)/dataRAPM$poss_def
  marginFull <- c(margin, margin2)
  marginFull[is.na(marginFull)] = 0
  marginFull[is.infinite(marginFull)] = 0
  additive_rating <- t(stintMatFull) %*% marginFull
  playerTotals <- filter(playerTotals, person_id %in% as.numeric(gsub("Def_ID|Off_ID", "",colnames(stintMatFull)[-1])))
  playerTotals <- arrange(playerTotals, person_id)
  W <- matrix(0, ncol = ncol(stintMatFull), nrow = ncol(stintMatFull))
  diag(W) <- c(1, playerTotals$poss_off, playerTotals$poss_def)
  adjMatrixAdditive <- W + adjMat
  possVec <- c(dataRAPM$poss_off, dataRAPM$poss_def)
  
  ##Get Rid of Low Minute Player Stints
  possCutoff <- summary(updatedPlayerMinutes$poss_tot)[2]
  LowPossPlayerTotals <- filter(updatedPlayerMinutes, poss_tot < possCutoff)
  lowPossCols <- c(paste0("Off_ID", LowPossPlayerTotals$person_id), paste0("Def_ID", LowPossPlayerTotals$person_id))
  stintMatFull <- stintMatFull[, -which(colnames(stintMatFull) %in% lowPossCols)]
  stintsToRemove <- which(rowSums(abs(stintMatFull)) != 11)
  stintMatFull <- stintMatFull[-stintsToRemove,]
  marginFull <- marginFull[-stintsToRemove]
  possFull <- possVec[-stintsToRemove]
  
  ##CV Ridge Regression
  x <- stintMatFull
  y <- marginFull
  lambdas <- seq(0, 4000, 100)
  fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
  cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, weights = possFull, nfolds = 10)
  opt_lambda <- cv_fit$lambda.min
  fit <- cv_fit$glmnet.fit
  summary(fit)
  results <- as.data.frame(do.call(rbind, strsplit(names(cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]), "_ID")))
  names(results) <- c("rapm_type", "person_id")
  results$RAPM <- cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]
  results <- merge(results, player_id_refs, by = "person_id", all.x = T)
  results <- spread(results, rapm_type, RAPM)
  results <- subset(results, select = -c(Constant))
  names(results)[which(names(results) == "Def")] = "D_RAPM"
  names(results)[which(names(results) == "Off")] = "O_RAPM"
  results$RAPM <- results$D_RAPM + results$O_RAPM
  results <- merge(results, updatedPlayerMinutes[,c("person_id", "mp")], by = "person_id", all.x = T)
  results <- arrange(results, -RAPM)
  
  opt_lambda2 <- 2000
  fit2 <- glmnet(x, y, alpha = 0, lambda = opt_lambda2)
  results2 <- as.data.frame(do.call(rbind, strsplit(row.names(fit2$beta), "_ID")))
  names(results2) <- c("rapm_type", "person_id")
  results2$RAPM <- as.vector(fit2$beta)
  results2 <- merge(results2, player_id_refs, by = "person_id", all.x = T)
  results2 <- spread(results2, rapm_type, RAPM)
  results2 <- subset(results2, select = -c(Constant))
  names(results2)[which(names(results2) == "Def")] = "D_RAPM"
  names(results2)[which(names(results2) == "Off")] = "O_RAPM"
  results2$RAPM <- results2$D_RAPM + results2$O_RAPM
  results2 <- merge(results2, playerTotals[,c("person_id", "mp")], by = "person_id", all.x = T)
  results2 <- arrange(results2, -RAPM)
}

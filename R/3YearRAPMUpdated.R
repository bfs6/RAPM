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
library(matlib)
library(plyr)
library(Matrix)



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
apm_player_stats <- sqlQuery(mydb, "select * from dbo.apm_player_stats
                                          where 1 = 1
                                          and league_id = 00
                                          and game_id = 'All'
                                          and stat_type = 'Totals'")
odbcClose(mydb)



####Clean Data####
nba_lineup_game_dim$season_num <- as.numeric(gsub("NBA_", "", nba_lineup_game_dim$season_name))
nba_lineup_game_dim <- remove_zero_var_cols(nba_lineup_game_dim)
nba_all_player_stats <- remove_zero_var_cols(nba_all_player_stats)
nba_all_player_stats_totals <- setDT(nba_all_player_stats)[, lapply(.SD, sum), by = c("person_id", "season_name"), .SDcols = c(names(nba_all_player_stats)[which(grepl("poss_", names(nba_all_player_stats)) == T)], "mp")]
apm_player_stats <- remove_zero_var_cols(apm_player_stats)


####Stint Evaluation####
RAPM_type = 3
RAPM_list1 <- list()
RAPM_list2 <- list()
seasons_loop_min = min(nba_lineup_game_dim$season_num) + (RAPM_type - 1)
seasons_loop_max = max(nba_lineup_game_dim$season_num)
seasons_loop = c(seasons_loop_min:seasons_loop_max)
####Start Stint Evaluation For Loop####
for(ssn in seasons_loop){
  print(ssn)
  ##Filter Down Data for Each Year
  if(RAPM_type == 1){
    dataRAPM <- filter(nba_lineup_game_dim, season_num == ssn & home_away == "Home")
  }else{
    dataRAPM <- filter(nba_lineup_game_dim, season_num %in% c((ssn - (RAPM_type - 1)):ssn) & home_away == "Home")
  }
  dataRAPM <- arrange(dataRAPM, game_id, period, -game_clock_int_start)
  dataRAPM$poss_tot[which(dataRAPM$pts > 0 & dataRAPM$poss_tot == 0)] = rep(1, length(which(dataRAPM$pts > 0 & dataRAPM$poss_tot == 0)))
  dataRAPM$poss_tot[which(dataRAPM$opp_pts > 0 & dataRAPM$poss_tot == 0)] = rep(1, length(which(dataRAPM$opp_pts > 0 & dataRAPM$poss_tot == 0)))
  #dataRAPM <- filter(dataRAPM, poss_tot > 0 & poss_off > 0 & poss_def > 0)
  dataRAPM <- filter(dataRAPM, poss_tot > 0)
  
  ##Filter Out Garbage Time Stints
  #First Garbage Time Rule
  garbage_time_ind1_games <- dataRAPM %>% group_by(game_id) %>% filter(period == 4 & game_clock_int_start <= 72000 & game_clock_int_start > 54000) %>% mutate(score_end = (opp_score_end - score_end), score_start = (opp_score_start - score_start)) %>% select(game_id, score_end, score_start)
  garbage_time_ind1_games <- unique(garbage_time_ind1_games[c(which(garbage_time_ind1_games$score_start >= 25 & garbage_time_ind1_games$score_end <= 25), which(garbage_time_ind1_games$score_start <= -25 & garbage_time_ind1_games$score_end >= -25)), "game_id"])
  garbage_time_ind1 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 72000 & dataRAPM$game_clock_int_start > 54000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 25 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 25)
  
  #Second Garbage Time Rule
  garbage_time_ind2_games <- dataRAPM %>% group_by(game_id) %>% filter(period == 4 & game_clock_int_start <= 54000 & game_clock_int_start > 36000) %>% mutate(score_end = (opp_score_end - score_end), score_start = (opp_score_start - score_start)) %>% select(game_id, score_end, score_start)
  garbage_time_ind2_games <- unique(garbage_time_ind2_games[c(which(garbage_time_ind2_games$score_start >= 20 & garbage_time_ind2_games$score_end <= 20), which(garbage_time_ind2_games$score_start <= -20 & garbage_time_ind2_games$score_end >= -20)), "game_id"])
  garbage_time_ind2 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 54000 & dataRAPM$game_clock_int_start > 36000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 20 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 20) 
  
  #Third Garbage Time Rule
  garbage_time_ind3_games <- dataRAPM %>% group_by(game_id) %>% filter(period == 4 & game_clock_int_start <= 36000) %>% mutate(score_end = (opp_score_end - score_end), score_start = (opp_score_start - score_start)) %>% select(game_id, score_end, score_start)
  garbage_time_ind3_games <- unique(garbage_time_ind3_games[c(which(garbage_time_ind3_games$score_start >= 10 & garbage_time_ind3_games$score_end <= 10), which(garbage_time_ind3_games$score_start <= -10 & garbage_time_ind3_games$score_end >= -10)), "game_id"])
  garbage_time_ind3 <- which(dataRAPM$period == 4 & dataRAPM$game_clock_int_start <= 36000 & 
                               abs(dataRAPM$opp_score_end - dataRAPM$score_end) >= 10 & abs(dataRAPM$opp_score_start - dataRAPM$score_start) >= 10) 
  
  #Combine and Filter
  garbage_time_games_exclude <- unique(c(garbage_time_ind1_games$game_id, garbage_time_ind2_games$game_id, garbage_time_ind3_games$game_id))
  garbage_time_games_exclude_ind <- which(dataRAPM$game_id %in% garbage_time_games_exclude)
  garbage_time_ind <- unique(c(garbage_time_ind1, garbage_time_ind2, garbage_time_ind3))
  garbage_time_ind <- garbage_time_ind[-which(garbage_time_ind %in% garbage_time_games_exclude_ind)]
  dataRAPM <- dataRAPM[-garbage_time_ind, ]
  dataRAPM <- filter(dataRAPM, is.na(score_start) == F)
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
  
  ##Find Low Min Players
  playerTotals <- filter(nba_all_player_stats_totals, season_name == paste0("NBA_", ssn))
  # minCutoff <- 250
  # HighMinPlayerTotals <- filter(playerTotals, mp >= minCutoff)
  # LowMinPlayerTotals <- filter(playerTotals, mp < minCutoff)
  
  ##Filter Out All Star Stuff
  team_ref_ids <- unique(dataRAPM[,c("team_abbr", "home_team_id")])
  team_ref_ids <- na.omit(team_ref_ids[1:30,])
  row.names(team_ref_ids) <- NULL
  team_ref_ids <- arrange(team_ref_ids, home_team_id)
  dataRAPM <- filter(dataRAPM, home_team_id %in% team_ref_ids$home_team_id)
  team_ref_ids <- setDT(team_ref_ids)[, .SD[1], by = "home_team_id"]
  team_ref_ids <- as.data.frame(team_ref_ids)
  
  ##Add Home Court Features (Home Court, Days Rest, Score Diff, Time Remaining)
  homeCourtMat <- model.matrix(~ as.character(home_team_id) + 0, dataRAPM)
  colnames(homeCourtMat) <- paste0("HomeCourt_", team_ref_ids$team_abbr)
  homeCourtMat2 <- homeCourtMat * -1
  homeCourtMatFull <- rbind(homeCourtMat, homeCourtMat2)
  
  ##Add Days Rest Features
  dataRAPM$team_days_rest[which(dataRAPM$team_days_rest < 0)] = rep(0, length(which(dataRAPM$team_days_rest < 0)))
  dataRAPM$opp_team_days_rest[which(dataRAPM$opp_team_days_rest < 0)] = rep(0, length(which(dataRAPM$opp_team_days_rest < 0)))
  daysRestMat <- model.matrix(~ as.character(team_days_rest) + 0, dataRAPM)
  oppDaysRestMat <- model.matrix(~ as.character(opp_team_days_rest) + 0, dataRAPM)
  oppDaysRestMat <- oppDaysRestMat * -1
  colnames(daysRestMat) <- gsub("as.character[(]team_days_rest[)]", "DaysRest", colnames(daysRestMat))
  colnames(oppDaysRestMat) <- gsub("as.character[(]opp_team_days_rest[)]", "OppDaysRest", colnames(oppDaysRestMat))
  daysRestMatFull <- cbind(daysRestMat, oppDaysRestMat)
  
  daysRestMat2 <- oppDaysRestMat * -1
  oppDaysRestMat2 <- daysRestMat * -1
  colnames(daysRestMat2) <- gsub("OppDaysRest", "DaysRest", colnames(daysRestMat2))
  colnames(oppDaysRestMat2) <- gsub("DaysRest", "OppDaysRest", colnames(oppDaysRestMat2))
  daysRestMatFull2 <- cbind(daysRestMat2, oppDaysRestMat2)
  
  daysRestMatFull <- rbind.fill.matrix(daysRestMatFull, daysRestMatFull2)
  daysRestMatFull <- daysRestMatFull[,sort(colnames(daysRestMatFull))]
  daysRestMatFull[is.na(daysRestMatFull)] = 0
  
  ##Score Difference Features
  scoreDiff <- (dataRAPM$score_start - dataRAPM$opp_score_start + dataRAPM$score_end - dataRAPM$opp_score_end)/2
  scoreDiff2 <- scoreDiff * -1
  scoreDiffBins <- quantile(c(scoreDiff, scoreDiff2), na.rm = T, seq(0, 1, 0.05))
  scoreDiffBins[length(scoreDiffBins)] <- scoreDiffBins[length(scoreDiffBins)] + 1
  scoreDiffMat <- matrix(0, ncol = length(scoreDiffBins) - 1, nrow = nrow(dataRAPM))
  scoreDiffMat2 <- matrix(0, ncol = length(scoreDiffBins) - 1, nrow = nrow(dataRAPM))
  for(i in 1:ncol(scoreDiffMat)){
    scoreDiffMat[,i] <- as.numeric(scoreDiff >= scoreDiffBins[i] & scoreDiff < scoreDiffBins[i + 1])
    scoreDiffMat2[,i] <- as.numeric(scoreDiff2 >= scoreDiffBins[i] & scoreDiff2 < scoreDiffBins[i + 1])
  }
  scoreDiffMatFull <- rbind(scoreDiffMat, scoreDiffMat2)
  colnames(scoreDiffMatFull) <- paste0("ScoreDiff_", sapply(1:(length(scoreDiffBins) - 1), function(x) paste0(gsub("%","",names(scoreDiffBins)[x]), "-", names(scoreDiffBins)[x+1])))
  
  ##Time Remaining Features
  timeRemaining <- (dataRAPM$game_clock_int_end + dataRAPM$game_clock_int_start)/2/6000 + ifelse(4 - dataRAPM$period < 0, 0, 4 - dataRAPM$period) * 12
  timeRemainingFull <- rep(timeRemaining, 2)
  
  ##Absorb Extra Terms into Intercept
  homeCourtMatFull <- homeCourtMatFull[,-1]
  daysRestMatFull <- daysRestMatFull[, -c(1, (ncol(daysRestMatFull)/2 + 1))]
  scoreDiffMatFull <- scoreDiffMatFull[,-1]
  scoreTimeInt <- scoreDiffMatFull * timeRemainingFull
  colnames(scoreTimeInt) <- gsub("Score", "ScoreTime", colnames(scoreTimeInt))
  
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
  margin <- 100 * (dataRAPM$pts)/dataRAPM$poss_off
  margin2 <- 100 * (dataRAPM$opp_pts)/dataRAPM$poss_def
  marginFull <- c(margin, margin2)
  marginFull[is.na(marginFull)] = 0
  marginFull[is.infinite(marginFull)] = 0
  playerTotals <- filter(playerTotals, person_id %in% as.numeric(gsub("Def_ID|Off_ID", "",colnames(stintMatFull)[-1])))
  playerTotals <- arrange(playerTotals, person_id)
  possVec <- c(dataRAPM$poss_off, dataRAPM$poss_def)
  stintMatFull <- cbind(stintMatFull, homeCourtMatFull, daysRestMatFull, scoreDiffMatFull, scoreTimeInt)
  rm(homeCourtMatFull, homeCourtMat, homeCourtMat2, daysRestMatFull, daysRestMatFull2, scoreDiffMat, scoreDiffMat2, scoreDiffMatFull, scoreTimeInt, stintMat, stintMat2)
  
  ##Get Rid of Low Possession Player Stints
  possCutoff <- summary(updatedPlayerMinutes$poss_tot)[2]
  LowPossPlayerTotals <- filter(updatedPlayerMinutes, poss_tot < possCutoff)
  lowPossCols <- c(paste0("Off_ID", LowPossPlayerTotals$person_id), paste0("Def_ID", LowPossPlayerTotals$person_id))
  stintMatFull <- stintMatFull[, -which(colnames(stintMatFull) %in% lowPossCols)]
  playerIDVars <- colnames(stintMatFull)[which(grepl("Def_ID|Off_ID", colnames(stintMatFull)) == T)]
  stintsToRemove <- which(rowSums(abs(stintMatFull[,which(colnames(stintMatFull) %in% playerIDVars)])) != 10)
  stintMatFull <- stintMatFull[-stintsToRemove,]
  marginFull <- marginFull[-stintsToRemove]
  possFull <- possVec[-stintsToRemove]
  
  ##CV Ridge Regression
  x <- stintMatFull
  y <- marginFull
  lambdas <- seq(50, 4000, 50)
  fit <- glmnet(x, y, alpha = 0, lambda = lambdas, standardize = F, standardize.response = F)
  cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, weights = possFull, nfolds = 10, standardize = F, standardize.response = F)
  # cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, standardize = F, standardize.response = F)
  opt_lambda <- cv_fit$lambda.min
  #opt_lambda <- 50
  fit <- cv_fit$glmnet.fit
  summary(fit)
  allBetas <- cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]
  playerBetas <- allBetas[names(allBetas)[which(grepl("Off_ID|Def_ID", names(allBetas)) == T)]]
  results <- as.data.frame(do.call(rbind, strsplit(names(playerBetas), "_ID")))
  names(results) <- c("rapm_type", "person_id")
  results$RAPM <- playerBetas
  results <- merge(results, player_id_refs, by = "person_id", all.x = T)
  results <- spread(results, rapm_type, RAPM)
  # results <- subset(results, select = -c(Constant))
  names(results)[which(names(results) == "Def")] = "D_RAPM"
  names(results)[which(names(results) == "Off")] = "O_RAPM"
  results$RAPM <- results$D_RAPM + results$O_RAPM
  results <- merge(results, updatedPlayerMinutes[,c("person_id", "mp")], by = "person_id", all.x = T)
  results <- arrange(results, -RAPM)
  results$season_name <- rep(paste0("NBA_", ssn), nrow(results))
  
  ##Bayesian Ridge Calculation
  lambda <- 2222
  # lambda <- 4500
  # lambda <- 1500
  W <- Matrix(0, nrow = nrow(x), ncol = nrow(x))
  diag(W) <- possFull
  xTx <- t(x) %*% W %*% x
  xTy <- t(x) %*% W %*% y
  xTx <- t(x) %*% x
  xTy <- t(x) %*% y
  I <- diag(nrow(xTy))
  xTx_lambda <- xTx + lambda * I
  mean <- solve(xTx_lambda) %*% xTy
  playerMeanBetas <- mean[which(grepl("Off_ID|Def_ID", colnames(x)) == T)]
  meanDF <- data.frame(varID = colnames(x)[which(grepl("Off_ID|Def_ID", colnames(x)) == T)], RAPM = playerMeanBetas)
  row.names(meanDF) <- NULL
  meanDF$varID <- as.character(meanDF$varID)
  meanDF <- data.frame(rapm_type = do.call(rbind, strsplit(meanDF$varID, "_ID"))[,1],
                       person_id = as.numeric(do.call(rbind, strsplit(meanDF$varID, "_ID"))[,2]),
                       RAPM = meanDF$RAPM)
  meanDF <- merge(meanDF, player_id_refs, by = "person_id", all.x = T)
  meanDF <- spread(meanDF, rapm_type, RAPM)
  names(meanDF)[which(names(meanDF) == "Def")] = "D_RAPM"
  names(meanDF)[which(names(meanDF) == "Off")] = "O_RAPM"
  meanDF <- merge(meanDF, updatedPlayerMinutes[,c("person_id", "mp", "poss_off", "poss_def", "poss_tot")], by = "person_id", all.x = T)
  meanDF$RAPM <- meanDF$O_RAPM + meanDF$D_RAPM
  meanDF <- arrange(meanDF, -RAPM)
  
  sigma <- 3
  var <- lambda * sigma * solve(xTx_lambda)
  varDF <- data.frame(rapm_type = do.call(rbind, strsplit(names(diag(var)), "_ID"))[,1],
                      person_id = as.numeric(do.call(rbind, strsplit(names(diag(var)), "_ID"))[,2]),
                      VAR = diag(var))
  row.names(varDF) <- NULL
  varDF <- filter(varDF, rapm_type != "Constant")
  varDF <- filter(varDF, is.na(person_id) == F)
  varDF <- spread(varDF, rapm_type, VAR)
  names(varDF)[which(names(varDF) == "Def")] = "D_Var"
  names(varDF)[which(names(varDF) == "Off")] = "O_Var"
  RAPM_DF <- merge(meanDF, varDF, by = "person_id", all.x = T)
  RAPM_DF <- arrange(RAPM_DF, -RAPM)
  RAPM_DF$season_name <- rep(paste0("NBA_", ssn), nrow(RAPM_DF))
  RAPM_list1[[ssn]] <- RAPM_DF
  RAPM_list2[[ssn]] <- results
}
RAPM_full1 <- do.call(rbind, RAPM_list1)
RAPM_full2 <- do.call(rbind, RAPM_list2)
fwrite(RAPM_full1, "3YrRAPMV1.csv", sep = ",", row.names = F)
fwrite(RAPM_full2, "3YrRAPMV2.csv", sep = ",", row.names = F)




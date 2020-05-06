####Read in Libraries####
library(RODBC)
library(data.table)
library(dplyr)


####Read in Data####
oneYearRAPMv1 <- fread("1YrRAPMV1.csv", sep = ",", stringsAsFactors = F)
oneYearRAPMv2 <- fread("1YrRAPMV2.csv", sep = ",", stringsAsFactors = F)
threeYearRAPMv1 <- fread("3YrRAPMV1.csv", sep = ",", stringsAsFactors = F)
threeYearRAPMv2 <- fread("3YrRAPMV2.csv", sep = ",", stringsAsFactors = F)


####Merge Data####
##V1
oneYearRAPMv1 <- oneYearRAPMv1[,c("person_id", "full_name", "O_RAPM", "D_RAPM", "RAPM", "season_name"), with = F]
threeYearRAPMv1 <- threeYearRAPMv1[,c("person_id", "full_name", "O_RAPM", "D_RAPM", "RAPM", "season_name"), with = F]
names(oneYearRAPMv1)[which(names(oneYearRAPMv1) %in% c("O_RAPM", "D_RAPM", "RAPM"))] = paste0("one_year_", c("O_RAPM", "D_RAPM", "RAPM"))
names(threeYearRAPMv1)[which(names(threeYearRAPMv1) %in% c("O_RAPM", "D_RAPM", "RAPM"))] = paste0("three_year_", c("O_RAPM", "D_RAPM", "RAPM"))
RAPMv1 <- merge(oneYearRAPMv1, threeYearRAPMv1, by = c("person_id", "full_name", "season_name"), all.x = T)

##V2
oneYearRAPMv2 <- oneYearRAPMv2[,c("person_id", "full_name", "O_RAPM", "D_RAPM", "RAPM", "season_name"), with = F]
threeYearRAPMv2 <- threeYearRAPMv2[,c("person_id", "full_name", "O_RAPM", "D_RAPM", "RAPM", "season_name"), with = F]
names(oneYearRAPMv2)[which(names(oneYearRAPMv2) %in% c("O_RAPM", "D_RAPM", "RAPM"))] = paste0("one_year_", c("O_RAPM", "D_RAPM", "RAPM"))
names(threeYearRAPMv2)[which(names(threeYearRAPMv2) %in% c("O_RAPM", "D_RAPM", "RAPM"))] = paste0("three_year_", c("O_RAPM", "D_RAPM", "RAPM"))
RAPMv2 <- merge(oneYearRAPMv2, threeYearRAPMv2, by = c("person_id", "full_name", "season_name"), all.x = T)


####Connect to DB and Upload####
mydb <- odbcDriverConnect(connection = "Driver={SQL Server Native Client 11.0};server=PHX-SQL07\\BBOPS; database=BOps_DW;trusted_connection=yes;")
sqlDrop(mydb, "adhoc.bseif_RAPM_glmnet")
sqlSave(mydb, RAPMv2, "adhoc.bseif_RAPM_glmnet", rownames = F)
sqlDrop(mydb, "adhoc.bseif_RAPM_manual")
sqlSave(mydb, RAPMv1, "adhoc.bseif_RAPM_manual", rownames = F)
odbcClose(mydb)


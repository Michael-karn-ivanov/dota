data_setup <- function(file_name) {

  temp <- read.csv(file_name, stringsAsFactors=FALSE)
  temp$date <- as.Date(temp$date)
  temp$streak_date <- NA
  temp$streak_date <- temp$date - streak_time
  temp$streak1 <- NA
  temp$winstreak1 <- NA
  temp$winstreak_percent1 <- NA
  temp$winstreak_curr_opp1 <- NA
  temp$streak2 <- NA
  temp$winstreak2 <- NA
  temp$winstreak_percent2 <- NA
  temp$winstreak_curr_opp2 <- NA
  
  #считаем стрики по командам
  temp$streak1 <- apply (temp, 1, function(x)
    nrow(temp[temp$date >= x[8] & temp$radiant_id == x[3] & temp$match_id < x[1],]) + 
      nrow(temp[temp$date >= x[8] & temp$dire_id == x[3] & temp$match_id < x[1] ,]))
  
  temp$streak2 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[8] & temp$radiant_id == x[4] & temp$match_id < x[1],]) + 
      nrow(temp[temp$date >= x[8] & temp$dire_id == x[4] & temp$match_id < x[1] ,]))
  
  temp$winstreak1 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[8] & temp$radiant_id == x[3] & temp$match_id < x[1] & temp$winner == "radiant",]) + 
      nrow(temp[temp$date >= x[8] & temp$dire_id == x[3] & temp$match_id < x[1] & temp$winner == "dire",]))
  
  temp$winstreak2 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[8] & temp$radiant_id == x[4] & temp$match_id < x[1] & temp$winner == "radiant",]) + 
      nrow(temp[temp$date >= x[8] & temp$dire_id == x[4] & temp$match_id < x[1] & temp$winner == "dire",]))
  
  #обнуляем слишком короткие стрики, как подозрение на некорректные данные в источнике
  temp[temp$streak1 < streak_val,"streak1"] <- NA
  temp[temp$streak2 < streak_val,"streak2"] <- NA
  temp[is.na(temp$streak1),"win_streak1"] <- NA
  temp[is.na(temp$streak2),"win_streak2"] <- NA
  
  #считаем пропорцию побед в стрике
  temp_test <- temp[,c(9,10,13,14)]
  temp_test <- as.matrix(temp_test)
  temp$winstreak_percent1 <- apply (temp_test, 1, function(x) x[2]/x[1])
  temp$winstreak_percent2 <- apply (temp_test, 1, function(x) x[4]/x[3])
  
  temp <- temp[(temp$date>Sys.Date()-time_frame),]
  
  temp$winner_id <- NA
  temp[temp$winner == "dire", "winner_id"] <- temp[temp$winner == "dire", "dire_id"]
  temp[temp$winner == "radiant", "winner_id"] <- temp[temp$winner == "radiant", "radiant_id"]
  temp$loser_id <- NA
  temp[temp$winner == "dire", "loser_id"] <- temp[temp$winner == "dire", "radiant_id"]
  temp[temp$winner == "radiant", "loser_id"] <- temp[temp$winner == "radiant", "dire_id"]
  
  temp$winner_winstreak <- NA
  temp$loser_winstreak <- NA
  temp$winner_winstreak_per <- NA
  temp$loser_winstreak_per <- NA
  
  #расписываем стрики по победителям и проигравшим
  temp[temp$winner == "dire", "winner_winstreak"] <- temp[temp$winner == "dire", "winstreak2"]
  temp[temp$winner == "dire", "winner_winstreak_per"] <- temp[temp$winner == "dire", "winstreak_percent2"]
  temp[temp$winner == "radiant", "winner_winstreak"] <- temp[temp$winner == "radiant", "winstreak1"]
  temp[temp$winner == "radiant", "winner_winstreak_per"] <- temp[temp$winner == "radiant", "winstreak_percent1"]
  temp[temp$winner == "dire", "loser_winstreak"] <- temp[temp$winner == "dire", "winstreak1"]
  temp[temp$winner == "dire", "loser_winstreak_per"] <- temp[temp$winner == "dire", "winstreak_percent1"]
  temp[temp$winner == "radiant", "loser_winstreak"] <- temp[temp$winner == "radiant", "winstreak2"]
  temp[temp$winner == "radiant", "loser_winstreak_per"] <- temp[temp$winner == "radiant", "winstreak_percent2"]
  
  
  levels(temp[,"winner_id"]) <- unique(c(temp[,"winner_id"], temp[,"loser_id"]))
  levels(temp[,"loser_id"]) <- unique(c(temp[,"winner_id"], temp[,"loser_id"]))
  
  temp$result <- rep (1, nrow (temp))
  
  temp <- temp[!is.na (temp$winner_id),]
  temp <- temp[!is.na (temp$loser_id),]
  
  #несколько раз итеративно, так как могут быть пересечения
  temp <- temp[temp$loser_id %in% temp$winner_id, ]
  temp <- temp[temp$winner_id %in% temp$loser_id, ]
  temp <- temp[temp$loser_id %in% temp$winner_id, ]
  temp <- temp[temp$winner_id %in% temp$loser_id, ]
  temp <- temp[temp$loser_id %in% temp$winner_id, ]
  temp <- temp[temp$winner_id %in% temp$loser_id, ]
  temp <- temp[temp$loser_id %in% temp$winner_id, ]
  temp <- temp[temp$winner_id %in% temp$loser_id, ]
  
  return (temp)
}


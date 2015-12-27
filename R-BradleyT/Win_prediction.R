library(readr)
library(BradleyTerry2)
library(reshape)
#source("Kelly.R")

inv_logit <- function(p) {
  exp(p) / (1 + exp(p))
}

prob_BT <- function(ability_1, ability_2) {
  inv_logit(ability_1 - ability_2)
}


Pr = 1.4 #граница отсечения значения ability/s.e. Команды со значением меньше исключаются.
edge = 0 #граница значения Келли, выше которого ставим
time_frame = 90 #период анализа данных
#bigprize = 1000000 #нижняя граница категории турниров с большими призами, USD
#midprize = 200000 #нижняя граница категории турниров со средними призами, USD
streak_time = 5 #длительность периода, победы в котором считаются влияющими на результат
streak_val = streak_time #кол-во матчей в стрике за Х дней, говорящее о полноте данных



matches <- read.csv("matches.csv", stringsAsFactors=FALSE)
matches$date <- as.Date(matches$date)
class(matches$date)
matches$streak_date <- NA
matches$streak_date <- matches$date - streak_time
matches$streak1 <- NA
matches$winstreak1 <- NA
matches$winstreak_percent1 <- NA
matches$winstreak_curr_opp1 <- NA
matches$streak2 <- NA
matches$winstreak2 <- NA
matches$winstreak_percent2 <- NA
matches$winstreak_curr_opp2 <- NA


#считаем стрики по командам
matches$streak1 <- apply (matches, 1, function(x)
  nrow(matches[matches$date >= x[8] & matches$radiant_id == x[3] & matches$match_id < x[1],]) + 
  nrow(matches[matches$date >= x[8] & matches$dire_id == x[3] & matches$match_id < x[1] ,]))

matches$streak2 <- apply (matches, 1, function(x) 
  nrow(matches[matches$date >= x[8] & matches$radiant_id == x[4] & matches$match_id < x[1],]) + 
    nrow(matches[matches$date >= x[8] & matches$dire_id == x[4] & matches$match_id < x[1] ,]))

matches$winstreak1 <- apply (matches, 1, function(x) 
  nrow(matches[matches$date >= x[8] & matches$radiant_id == x[3] & matches$match_id < x[1] & matches$winner == "radiant",]) + 
    nrow(matches[matches$date >= x[8] & matches$dire_id == x[3] & matches$match_id < x[1] & matches$winner == "dire",]))

matches$winstreak2 <- apply (matches, 1, function(x) 
  nrow(matches[matches$date >= x[8] & matches$radiant_id == x[4] & matches$match_id < x[1] & matches$winner == "radiant",]) + 
    nrow(matches[matches$date >= x[8] & matches$dire_id == x[4] & matches$match_id < x[1] & matches$winner == "dire",]))

#обнуляем слишком короткие стрики, как подозрение на некорректные данные в источнике
matches[matches$streak1 < streak_val,"streak1"] <- NA
matches[matches$streak2 < streak_val,"streak2"] <- NA
matches[is.na(matches$streak1),"win_streak1"] <- NA
matches[is.na(matches$streak2),"win_streak2"] <- NA

#считаем пропорцию побед в стрике
matches_test <- matches[,c(9,10,13,14)]
matches_test <- as.matrix(matches_test)
matches$winstreak_percent1 <- apply (matches_test, 1, function(x) x[2]/x[1])
matches$winstreak_percent2 <- apply (matches_test, 1, function(x) x[4]/x[3])

matches <- matches[(matches$date>Sys.Date()-time_frame),]

#leagues <- read.csv("leagues.csv", stringsAsFactors=TRUE)

#категоризируем призы
#leagues$prize <- substring(leagues$prize,2)
#leagues$prize <- gsub(',','',leagues$prize)
#leagues$prize <- as.numeric(leagues$prize)
#leagues$prize_size <- NA
#leagues[is.na(leagues$prize),]$prize <- 0
#leagues$prize_size <- 'Small'
#leagues[leagues$prize > midprize,]$prize_size <- 'Mid'
#leagues[leagues$prize > bigprize,]$prize_size <- 'Big'

#matches <- merge(matches, leagues)

matches$winner_id <- NA
matches[matches$winner == "dire", "winner_id"] <- matches[matches$winner == "dire", "dire_id"]
matches[matches$winner == "radiant", "winner_id"] <- matches[matches$winner == "radiant", "radiant_id"]
matches$loser_id <- NA
matches[matches$winner == "dire", "loser_id"] <- matches[matches$winner == "dire", "radiant_id"]
matches[matches$winner == "radiant", "loser_id"] <- matches[matches$winner == "radiant", "dire_id"]

matches$winner_winstreak <- NA
matches$loser_winstreak <- NA
matches$winner_winstreak_per <- NA
matches$loser_winstreak_per <- NA

#расписываем стрики по победителям и проигравшим
matches[matches$winner == "dire", "winner_winstreak"] <- matches[matches$winner == "dire", "winstreak2"]
matches[matches$winner == "dire", "winner_winstreak_per"] <- matches[matches$winner == "dire", "winstreak_percent2"]
matches[matches$winner == "radiant", "winner_winstreak"] <- matches[matches$winner == "radiant", "winstreak1"]
matches[matches$winner == "radiant", "winner_winstreak_per"] <- matches[matches$winner == "radiant", "winstreak_percent1"]
matches[matches$winner == "dire", "loser_winstreak"] <- matches[matches$winner == "dire", "winstreak1"]
matches[matches$winner == "dire", "loser_winstreak_per"] <- matches[matches$winner == "dire", "winstreak_percent1"]
matches[matches$winner == "radiant", "loser_winstreak"] <- matches[matches$winner == "radiant", "winstreak2"]
matches[matches$winner == "radiant", "loser_winstreak_per"] <- matches[matches$winner == "radiant", "winstreak_percent2"]


levels(matches[,"winner_id"]) <- unique(c(matches[,"winner_id"], matches[,"loser_id"]))
levels(matches[,"loser_id"]) <- unique(c(matches[,"winner_id"], matches[,"loser_id"]))

matches$result <- rep (1, nrow (matches))

matches <- matches[!is.na (matches$winner_id),]
matches <- matches[!is.na (matches$loser_id),]

matches <- matches[matches$loser_id %in% matches$winner_id, ]
matches <- matches[matches$winner_id %in% matches$loser_id, ]
matches <- matches[matches$loser_id %in% matches$winner_id, ]
matches <- matches[matches$winner_id %in% matches$loser_id, ]
matches <- matches[matches$loser_id %in% matches$winner_id, ]
matches <- matches[matches$winner_id %in% matches$loser_id, ]
matches <- matches[matches$loser_id %in% matches$winner_id, ]
matches <- matches[matches$winner_id %in% matches$loser_id, ]

winner.frame <- as.data.frame(cbind(matches$winner_id, matches$match_id, matches$winner_winstreak, matches$winner_winstreak_per))
loser.frame <- as.data.frame(cbind(matches$loser_id, matches$match_id, matches$loser_winstreak, matches$loser_winstreak_per))
#winner.frame <- as.data.frame(cbind(matches$winner_id, matches$match_id))
#loser.frame <- as.data.frame(cbind(matches$loser_id, matches$match_id))
colnames(winner.frame) <- c("id", "match_id","winstreak", "winstreak_percent")
colnames(loser.frame) <- c("id", "match_id", "winstreak", "winstreak_percent")
#colnames(winner.frame) <- c("id", "match_id")
#colnames(loser.frame) <- c("id", "match_id")
winner.frame$match_id <- as.factor(winner.frame$match_id)
loser.frame$match_id <- as.factor(loser.frame$match_id)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)
#winner.frame$prize_size <- as.factor(winner.frame$prize_size)
#loser.frame$prize_size <- as.factor(loser.frame$prize_size)

#модель с учетом стриков
summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame, formula = ~ id + winstreak + winstreak_percent,data = matches, 
                               id = "id"))

#summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
#                               player2 = loser.frame, formula = ~ prize_size[match_id]*id[id] + (1|id), data = matches, 
#                               id = "id", br = TRUE))

dotamatch.output <- data.frame(BTabilities(dotamatch.model))
dotamatch.output <- dotamatch.output[(dotamatch.output$ability/dotamatch.output$s.e.>Pr & !is.na(dotamatch.output$s.e.)),]
dotamatch.output <- dotamatch.output[!is.na (dotamatch.output$ability),]
team_names <- rownames(dotamatch.output)
dotamatch.abilities <- dotamatch.output$ability
names(dotamatch.abilities) <- team_names

dota_probs <- outer(dotamatch.abilities, dotamatch.abilities, prob_BT)
diag(dota_probs) <- 0
dota_probs <- melt(dota_probs)
colnames(dota_probs)[1] <- "Team1"
colnames(dota_probs)[2] <- "Team2"
head (dota_probs, 5)

#грузим коэффициенты и объединяем с рассчитанными вероятностями
coeffs <- read.csv("Coefs.csv", stringsAsFactors=FALSE)
coeffs <- merge(coeffs, dota_probs)

#считаем Келли
coeffs_test <- coeffs[,c(5,6,11)]
coeffs_test <- as.matrix(coeffs_test)
coeffs$answ1 <- NA
coeffs$answ2 <- NA
coeffs$answ1 <- apply (coeffs_test, 1, function(x) (x[3] * x[1] - 1)/(x[1] - 1))
coeffs$answ2 <- apply (coeffs_test, 1, function(x) ((1-x[3]) * x[2] - 1)/(x[2] - 1))

#удаляем не плюсовые строки
coeffs <- coeffs[!(coeffs$answ1<edge & coeffs$answ2<edge),]
coeffs <- coeffs[!(is.na(coeffs$Match_Id)),]





library(readr)
library(BradleyTerry2)
library(reshape)
source("Data setup.R")

inv_logit <- function(p) {
  exp(p) / (1 + exp(p))
}

prob_BT <- function(ability_1, ability_2) {
  inv_logit(ability_1 - ability_2)
}

#параметры
Pr = 1.4 #граница отсечения значения ability/s.e. Команды со значением меньше исключаются.
edge = 0 #граница значения Келли, выше которого ставим
time_frame = 120 #период анализа данных
streak_time = 5 #длительность периода, победы в котором считаются влияющими на результат
streak_val = 1 #кол-во матчей в стрике за Х дней, говорящее о полноте данных


#обработка данных
#matches <- data_setup("matches.csv")

#тут уже должны быть коэффициенты
  temp <- read.csv("matches.csv", stringsAsFactors=FALSE)
  temp$coeff1 <- NA
  temp$coeff2 <- NA
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
  temp$winner_winstreak <- NA
  temp$loser_winstreak <- NA
  temp$winner_winstreak_per <- NA
  temp$loser_winstreak_per <- NA
  
  #считаем кол-во сыгранных каждой командой матчей за Х дней до текущего матча
  temp$streak1 <- apply (temp, 1, function(x)
    nrow(temp[temp$date >= x[10] & temp$radiant_id == x[3] & temp$match_id < x[1],]) + 
      nrow(temp[temp$date >= x[10] & temp$dire_id == x[3] & temp$match_id < x[1] ,]))
  
  temp$streak2 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[10] & temp$radiant_id == x[4] & temp$match_id < x[1],]) + 
      nrow(temp[temp$date >= x[10] & temp$dire_id == x[4] & temp$match_id < x[1] ,]))
 
  #считаем кол-во выигранных каждой командой матчей за Х дней до текущего матча 
  temp$winstreak1 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[10] & temp$radiant_id == x[3] & temp$match_id < x[1] & temp$winner == "radiant",]) + 
      nrow(temp[temp$date >= x[10] & temp$dire_id == x[3] & temp$match_id < x[1] & temp$winner == "dire",]))
  
  temp$winstreak2 <- apply (temp, 1, function(x) 
    nrow(temp[temp$date >= x[10] & temp$radiant_id == x[4] & temp$match_id < x[1] & temp$winner == "radiant",]) + 
      nrow(temp[temp$date >= x[10] & temp$dire_id == x[4] & temp$match_id < x[1] & temp$winner == "dire",]))
  
  #обнуляем слишком короткие стрики, как подозрение на некорректные данные в источнике
  temp[temp$streak1 < streak_val,"streak1"] <- NA
  temp[temp$streak2 < streak_val,"streak2"] <- NA
  temp[is.na(temp$streak1),"win_streak1"] <- NA
  temp[is.na(temp$streak2),"win_streak2"] <- NA
  
  #считаем пропорцию побед в стрике
  temp_test <- temp[,c(11,12,15,16)]
  temp_test <- as.matrix(temp_test)
  temp$winstreak_percent1 <- apply (temp_test, 1, function(x) x[2]/x[1])
  temp$winstreak_percent2 <- apply (temp_test, 1, function(x) x[4]/x[3])
  
  #убираем старые данные
  temp <- temp[(temp$date>Sys.Date()-time_frame),]
  
  #определяем победителя каждого матча
  temp$winner_id <- NA
  temp[temp$winner == "dire", "winner_id"] <- temp[temp$winner == "dire", "dire_id"]
  temp[temp$winner == "radiant", "winner_id"] <- temp[temp$winner == "radiant", "radiant_id"]
  temp$loser_id <- NA
  temp[temp$winner == "dire", "loser_id"] <- temp[temp$winner == "dire", "radiant_id"]
  temp[temp$winner == "radiant", "loser_id"] <- temp[temp$winner == "radiant", "dire_id"]
  
  #расписываем стрики по победителям и проигравшим (вместо команд 1 и 2)
  temp[temp$winner == "dire", "winner_winstreak"] <- temp[temp$winner == "dire", "winstreak2"]
  temp[temp$winner == "dire", "winner_winstreak_per"] <- temp[temp$winner == "dire", "winstreak_percent2"]
  temp[temp$winner == "radiant", "winner_winstreak"] <- temp[temp$winner == "radiant", "winstreak1"]
  temp[temp$winner == "radiant", "winner_winstreak_per"] <- temp[temp$winner == "radiant", "winstreak_percent1"]
  temp[temp$winner == "dire", "loser_winstreak"] <- temp[temp$winner == "dire", "winstreak1"]
  temp[temp$winner == "dire", "loser_winstreak_per"] <- temp[temp$winner == "dire", "winstreak_percent1"]
  temp[temp$winner == "radiant", "loser_winstreak"] <- temp[temp$winner == "radiant", "winstreak2"]
  temp[temp$winner == "radiant", "loser_winstreak_per"] <- temp[temp$winner == "radiant", "winstreak_percent2"]
  
  #создаем уровни для значений Id команд
  levels(temp[,"winner_id"]) <- unique(c(temp[,"winner_id"], temp[,"loser_id"]))
  levels(temp[,"loser_id"]) <- unique(c(temp[,"winner_id"], temp[,"loser_id"]))
  
  temp$result <- rep (1, nrow (temp))
  
  #чистим матчи с неполными данными
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

#копируем в постоянную таблицу
matches <- temp

#создаем фреймы для модели
winner.frame <- as.data.frame(cbind(matches$date, matches$winner_id, matches$match_id, matches$winner_winstreak, matches$winner_winstreak_per))
loser.frame <- as.data.frame(cbind(matches$date, matches$loser_id, matches$match_id, matches$loser_winstreak, matches$loser_winstreak_per))
colnames(winner.frame) <- c("date", "id", "match_id","winstreak", "winstreak_percent")
colnames(loser.frame) <- c("date", "id", "match_id", "winstreak", "winstreak_percent")
winner.frame$match_id <- as.factor(winner.frame$match_id)
loser.frame$match_id <- as.factor(loser.frame$match_id)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

min_date <- as.Date("2015/12/01")
max_date <- as.Date("2015/12/15")

#пока так, проверяем работу цикла
#min_date = max_date

days <- seq(from=min_date, to=max_date,by='days' )
matches$value1 <- NA

#для каждого дня прогоняем модель на данных до этого дня и считаем вероятности матчей этого дня
for (i in seq_along(days))
{
#print (days[i])}
winner.frame.temp <- winner.frame [(winner.frame$date < days[i]),]
loser.frame.temp <- loser.frame [(loser.frame$date < days[i]),]
matches.temp <- matches [(matches$date < days[i]),]

#модель с учетом стриков
summary(dotamatch.model <- BTm(result, player1 = winner.frame.temp, 
                               player2 = loser.frame.temp, formula = ~ id + winstreak + winstreak_percent,data = matches.temp, 
                               id = "id"))

dotamatch.output <- data.frame(BTabilities(dotamatch.model))
dotamatch.output <- dotamatch.output[(dotamatch.output$ability/dotamatch.output$s.e.>Pr & !is.na(dotamatch.output$s.e.)),]
dotamatch.output <- dotamatch.output[!is.na (dotamatch.output$ability),]
team_names <- rownames(dotamatch.output)
dotamatch.abilities <- dotamatch.output$ability
names(dotamatch.abilities) <- team_names

dota_probs <- outer(dotamatch.abilities, dotamatch.abilities, prob_BT)
diag(dota_probs) <- 0
dota_probs <- melt(dota_probs)
colnames(dota_probs)[1] <- "radiant_id"
colnames(dota_probs)[2] <- "dire_id"
#head (dota_probs, 100)

matches.date <- matches[(matches$date == days[i]),]
matches.date <- merge (matches.date, dota_probs)
matches.date <- matches.date[,c(3,29)]
colnames(matches.date) <- c("match_id", "temp_value")
matches <- merge (matches, matches.date, all.x=TRUE)
matches[!is.na(matches$temp_value),"value1"] <- matches[!is.na(matches$temp_value),"temp_value"]
matches$temp_value <- NULL
}

#посчитали вероятности победы radiant, теперь то же самое для dire
matches_test <- matches[,c(28)]
matches_test <- as.matrix(matches_test)
matches$value2 <- NA
matches$value2 <- apply (matches_test, 1, function(x) (1-x[1]))

#считаем Келли
coeffs_test <- matches[,c(8,9,28,29)]
coeffs_test <- as.matrix(coeffs_test)
coeffs$answ1 <- NA
coeffs$answ2 <- NA
coeffs$answ1 <- apply (coeffs_test, 1, function(x) (x[3] * x[1] - 1)/(x[1] - 1))
coeffs$answ2 <- apply (coeffs_test, 1, function(x) (x[4] * x[2] - 1)/(x[2] - 1))

#удаляем не плюсовые строки
coeffs <- coeffs[!(coeffs$answ1<edge & coeffs$answ2<edge),]
coeffs <- coeffs[!(is.na(coeffs$Match_Id)),]





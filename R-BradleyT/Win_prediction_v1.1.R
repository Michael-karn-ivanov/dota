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
Pr = 0 #граница отсечения значения ability/s.e. Команды со значением меньше исключаются (1.4)
edge = 0 #граница значения Келли, выше которого ставим
time_frame = 120 #период анализа данных
streak_time = 5 #длительность периода, победы в котором считаются влияющими на результат
streak_val = 0 #кол-во матчей в стрике за Х дней, говорящее о полноте данных

files = list.files(path = "D:/calculations/coeffgrabber", pattern="*.csv")
setwd("D:/calculations/coeffgrabber")
EGB <- read.csv("EGB.csv", header = FALSE, stringsAsFactors=FALSE)
for (i in 1:length(files)) 
{
  EGB_temp <- read.csv(files[i], header = FALSE, stringsAsFactors=FALSE)
  EGB <- merge(EGB, EGB_temp, all=TRUE)
}
rm(EGB_temp)


#обработка данных
EGB <- EGB[,c(1,10,2,5,6,11,8,3,4,7,9)]
colnames(EGB) <- c("bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "map")
EGB$date <- as.Date(EGB$date)
EGB <- EGB[(EGB$date>"2016/03/01"),]

#данные по FB выносим отдельно
FB_data <- EGB[(EGB$bet_type == "FB" | EGB$bet_type == "First blood") & EGB$win != "draw",]
FB_data[FB_data$map == "First blood", "Map"] <- NA
FB_data <- FB_data[,c(1,2,3,4,5,6,7,8,9,11)]
FB_data$streak_date <- FB_data$date - streak_time

#EGB_temp <- EGB[EGB$Bet_type == "Total" | EGB$Bet_type == "GameResult",c(1,2,10,11)]
#EGB_temp$num <- apply (EGB_temp, 1, function(x) (sum(EGB_temp$series == x[2])))
#EGB_temp[EGB_temp$num == 1, c(3)] <- "GameResult"
#EGB_temp <- EGB_temp[EGB_temp$Bet_type == "GameResult", c(1,2,3,4)]
#EGB_temp$match <- EGB_temp$bet

#считаем кол-во сыгранных каждой командой матчей за Х дней до текущего матча
FB_data$streak1 <- apply (FB_data, 1, function(x)
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team1 == x[4] & FB_data$bet < x[1],]) + 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team2 == x[4] & FB_data$bet < x[1] ,]))
FB_data$streak2 <- apply (FB_data, 1, function(x) 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team1 == x[5] & FB_data$bet < x[1],]) + 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team2 == x[5] & FB_data$bet < x[1] ,]))
 
#считаем кол-во выигранных каждой командой матчей за Х дней до текущего матча 
FB_data$winstreak1 <- apply (FB_data, 1, function(x) 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team1 == x[4] & FB_data$bet < x[1] & FB_data$win == "win1",]) + 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team2 == x[4] & FB_data$bet < x[1] & FB_data$win == "win2",]))
FB_data$winstreak2 <- apply (FB_data, 1, function(x) 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team1 == x[5] & FB_data$bet < x[1] & FB_data$win == "win1",]) + 
  nrow(FB_data[FB_data$date >= x[11] & FB_data$team2 == x[5] & FB_data$bet < x[1] & FB_data$win == "win2",]))
  
#обнуляем слишком короткие стрики, как подозрение на некорректные данные в источнике
FB_data[FB_data$streak1 < streak_val,"streak1"] <- NA
FB_data[FB_data$streak2 < streak_val,"streak2"] <- NA
FB_data[is.na(FB_data$streak1),"winstreak1"] <- NA
FB_data[is.na(FB_data$streak2),"winstreak2"] <- NA
  
#считаем пропорцию побед в стрике
temp <- FB_data[,c(12,13,14,15)]
temp <- as.matrix(temp)
FB_data$winstreak_percent1 <- apply (temp, 1, function(x) x[3]/x[1])
FB_data$winstreak_percent2 <- apply (temp, 1, function(x) x[4]/x[2])
rm(temp)
  
#убираем старые данные
FB_data <- FB_data[(FB_data$date>Sys.Date()-time_frame),]
  
#определяем победителя каждого матча
FB_data[FB_data$win == "win1", "winner"] <- FB_data[FB_data$win == "win1", "team1"]
FB_data[FB_data$win == "win1", "loser"] <- FB_data[FB_data$win == "win1", "team2"]
FB_data[FB_data$win == "win2", "winner"] <- FB_data[FB_data$win == "win2", "team2"]
FB_data[FB_data$win == "win2", "loser"] <- FB_data[FB_data$win == "win2", "team1"]

#расписываем стрики по победителям и проигравшим (вместо команд 1 и 2)
FB_data[FB_data$win == "win1", "winner_winstreak"] <- FB_data[FB_data$win == "win1", "winstreak1"]
FB_data[FB_data$win == "win1", "winner_winstreak_per"] <- FB_data[FB_data$win == "win1", "winstreak_percent1"]
FB_data[FB_data$win == "win1", "loser_winstreak"] <- FB_data[FB_data$win == "win1", "winstreak2"]
FB_data[FB_data$win == "win1", "loser_winstreak_per"] <- FB_data[FB_data$win == "win1", "winstreak_percent2"]
FB_data[FB_data$win == "win2", "winner_winstreak"] <- FB_data[FB_data$win == "win2", "winstreak2"]
FB_data[FB_data$win == "win2", "winner_winstreak_per"] <- FB_data[FB_data$win == "win2", "winstreak_percent2"]
FB_data[FB_data$win == "win2", "loser_winstreak"] <- FB_data[FB_data$win == "win2", "winstreak1"]
FB_data[FB_data$win == "win2", "loser_winstreak_per"] <- FB_data[FB_data$win == "win2", "winstreak_percent1"]

#создаем уровни для значений Id команд
levels(FB_data[,"winner"]) <- unique(c(FB_data[,"winner"], FB_data[,"loser"]))
levels(FB_data[,"loser"]) <- unique(c(FB_data[,"winner"], FB_data[,"loser"]))
  
FB_data$result <- rep (1, nrow (FB_data))

FB_pure_lose <- FB_data[, c(1,19)]
FB_pure_lose$num <- apply (FB_pure_lose, 1, function(x) (sum(FB_pure_lose$loser == x[2])))
FB_pure_lose$bet <- NULL
FB_pure_lose <- unique(FB_pure_lose)
colnames(FB_pure_lose) <- c("team", "loses")
FB_pure_win <- FB_data[, c(1,18)]
FB_pure_win$num <- apply (FB_pure_win, 1, function(x) (sum(FB_pure_win$winner == x[2])))
FB_pure_win$bet <- NULL
FB_pure_win <- unique(FB_pure_win)
colnames(FB_pure_win) <- c("team", "wins")

FB_WL <- merge (FB_pure_win, FB_pure_lose, all = TRUE)
rm (FB_pure_lose)
rm (FB_pure_win)

setwd("D:/calculations")
write.csv(FB_WL, "FB_WL.csv")







#несколько раз итеративно, так как могут быть пересечения
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]

#создаем фреймы для модели
winner.frame <- as.data.frame(cbind(FB_data$date, FB_data$winner, FB_data$bet, FB_data$winner_winstreak, FB_data$winner_winstreak_per))
loser.frame <- as.data.frame(cbind(FB_data$date, FB_data$loser, FB_data$bet, FB_data$loser_winstreak, FB_data$loser_winstreak_per))
colnames(winner.frame) <- c("date", "id", "bet","winstreak", "winstreak_percent")
colnames(loser.frame) <- c("date", "id", "bet", "winstreak", "winstreak_percent")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)
winner.frame$winstreak <- as.numeric(winner.frame$winstreak)
loser.frame$winstreak <- as.numeric(loser.frame$winstreak)
winner.frame$winstreak_percent <- as.numeric(winner.frame$winstreak_percent)
loser.frame$winstreak_percent <- as.numeric(loser.frame$winstreak_percent)

winner.frame[winner.frame$winstreak_percent == "NaN","winstreak_percent"] <- "0"
loser.frame[loser.frame$winstreak_percent == "NaN","winstreak_percent"] <- "0"

winner.frame <- winner.frame[,c(2,3)]
loser.frame <- loser.frame[,c(2,3)]
FB_data <- FB_data[,c(1,24)]


summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = FB_data, 
                               id = "id"))

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame, formula = ~ id + winstreak + winstreak_percent,data = FB_data, 
                               id = "id"))









min_date <- as.Date("2016/02/15")
max_date <- Sys.Date()
days <- seq(from=min_date, to=max_date,by='days')

#для каждого дня прогоняем модель на данных до этого дня и считаем вероятности матчей этого дня
for (i in seq_along(days))
{
#print (days[i])}
winner.frame.temp <- winner.frame [(winner.frame$date < days[i]),]
loser.frame.temp <- loser.frame [(loser.frame$date < days[i]),]
FB_data.temp <- FB_data [(FB_data$date < days[i]),]

#модель с учетом стриков
summary(dotamatch.model <- BTm(result, player1 = winner.frame.temp, 
                               player2 = loser.frame.temp, formula = ~ id + winstreak + winstreak_percent,data = FB_data.temp, 
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
colnames(dota_probs)[1] <- "team1"
colnames(dota_probs)[2] <- "team2"
#head (dota_probs, 100)

temp <- FB_data[(FB_data$date == days[i]),]
temp <- merge (temp, dota_probs)
temp <- temp[,c(3,29)]
colnames(temp) <- c("match", "temp_value")
FB_data <- merge (FB_data, temp, all.x=TRUE)
FB_data[!is.na(FB_data$temp_value),"value1"] <- FB_data[!is.na(FB_data$temp_value),"temp_value"]
FB_data$temp_value <- NULL
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
coeffs <- coeffs[!(is.na(coeffs$Match)),]





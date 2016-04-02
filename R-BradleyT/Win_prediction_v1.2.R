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

prob_FB <- function(FB_1, FB_2) {
  (FB_1 + 1 - FB_2)/2
}

#параметры
Pr = 1.035 #граница отсечения значения ability/s.e. Команды со значением меньше исключаются (1.4)
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

setwd("C:/Users/������/Dropbox")
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
winner.frame <- as.data.frame(cbind(FB_data$winner, FB_data$bet))
loser.frame <- as.data.frame(cbind(FB_data$loser, FB_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = FB_data, 
                               id = "id"))

dotamatch.output <- data.frame(BTabilities(dotamatch.model))
dotamatch.output <- dotamatch.output[!is.na (dotamatch.output$ability),]
dotamatch.output$Pr <- apply (dotamatch.output, 1, function(x) (x[1] /x[2]))
dotamatch.output$Pr <- abs(dotamatch.output$Pr)
dotamatch.output <- dotamatch.output[(dotamatch.output$Pr>Pr & !is.na(dotamatch.output$s.e.)),]
team_names <- rownames(dotamatch.output)
dotamatch.abilities <- dotamatch.output$ability
names(dotamatch.abilities) <- team_names

dota_probs <- outer(dotamatch.abilities, dotamatch.abilities, prob_BT)
diag(dota_probs) <- 0
dota_probs <- melt(dota_probs)
colnames(dota_probs)[1] <- "team1"
colnames(dota_probs)[2] <- "team2"


FB_WL[is.na(FB_WL$wins),"wins"] <- 0
FB_WL[is.na(FB_WL$loses),"loses"] <- 0
temp <- FB_WL[,c(2,3)]
temp <- as.matrix(temp)
FB_WL$share <- apply (temp, 1, function(x) (x[1]/(x[1]+x[2])))
FB_WL$total <- apply (temp, 1, function(x) (x[1]+x[2]))
rm(temp)
FB_WL[FB_WL$total < 3,"share"] <- 0.5


FB.shares <- FB_WL$share
names(FB.shares) <- FB_WL$team
FB_probs <- outer(FB.shares, FB.shares, prob_FB)
diag(FB_probs) <- 0
FB_probs <- melt(FB_probs)

colnames(FB_probs)[1] <- "team1"
colnames(FB_probs)[2] <- "team2"
colnames(FB_probs)[3] <- "Pr1"

temp <- FB_probs[,c(3)]
temp <- as.matrix(temp)
FB_probs$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
FB_probs$coeff1 <- apply (temp, 1, function(x) 1/x[1])
FB_probs$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))

write.csv(FB_probs, "FB_probs.csv")

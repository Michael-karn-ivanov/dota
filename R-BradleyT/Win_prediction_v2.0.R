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


Pr = 1.4 #граница отсечения отношения матожидания к стандартной ошибке
time_frame = 120 #временная граница анализа данных

#собираем данные из всех .csv за все дни
files = list.files(path = "D:/calculations/coeffgrabber/Data", pattern="*.csv")
setwd("D:/calculations/coeffgrabber/Data")
EGB <- read.csv("EGB.csv", header = FALSE, stringsAsFactors=FALSE)
for (i in 1:length(files)) 
{
  EGB_temp <- read.csv(files[i], header = FALSE, stringsAsFactors=FALSE)
  EGB <- merge(EGB, EGB_temp, all=TRUE)
}
rm(EGB_temp)


#немного чистим
EGB <- EGB[,c(1,10,2,5,6,11,8,3,4,7,9)]
colnames(EGB) <- c("bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "map")
EGB$date <- as.Date(EGB$date)
EGB <- EGB[(EGB$date>"2016/03/01"),]

#еще чистим
EGB[EGB$team1 == "!Rebels!","team1"] <- "Rebels"
EGB[EGB$team2 == "!Rebels!","team2"] <- "Rebels"
EGB[EGB$team1 == "BrooDMotherS","team1"] <- "BroodMothers"
EGB[EGB$team2 == "BrooDMotherS","team2"] <- "BroodMothers"
EGB[EGB$team1 == "The Mongolz","team1"] <- "TheMongolz"
EGB[EGB$team2 == "The Mongolz","team2"] <- "TheMongolz"

#отбираем нужные данные по FB
FB_data <- EGB[grep("lood", EGB$bet_type),]
FB_data_temp <- EGB[EGB$bet_type == "FB",]
FB_data <- merge(FB_data, FB_data_temp, all=TRUE)
FB_data <- FB_data[FB_data$win != "draw" ,]
rm(FB_data_temp)

FB_data <- FB_data[,c(1,2,3,4,5,6,7,8,9)]
FB_data <- FB_data[(FB_data$date>Sys.Date()-time_frame),]
  
#прописываем явно победителя и проигравшего
FB_data[FB_data$win == "win1", "winner"] <- FB_data[FB_data$win == "win1", "team1"]
FB_data[FB_data$win == "win1", "loser"] <- FB_data[FB_data$win == "win1", "team2"]
FB_data[FB_data$win == "win2", "winner"] <- FB_data[FB_data$win == "win2", "team2"]
FB_data[FB_data$win == "win2", "loser"] <- FB_data[FB_data$win == "win2", "team1"]

#задаем уровни
levels(FB_data[,"winner"]) <- unique(c(FB_data[,"winner"], FB_data[,"loser"]))
levels(FB_data[,"loser"]) <- unique(c(FB_data[,"winner"], FB_data[,"loser"]))
  
#результат - везде побела winner'а
FB_data$result <- rep (1, nrow (FB_data))

#делаем таблицу с кол-вом выигранных и проигранных FB для каждой команды
FB_pure_lose <- FB_data[, c(1,11)]
FB_pure_lose$num <- apply (FB_pure_lose, 1, function(x) (sum(FB_pure_lose$loser == x[2])))
FB_pure_lose$bet <- NULL
FB_pure_lose <- unique(FB_pure_lose)
colnames(FB_pure_lose) <- c("team", "loses")
FB_pure_win <- FB_data[, c(1,10)]
FB_pure_win$num <- apply (FB_pure_win, 1, function(x) (sum(FB_pure_win$winner == x[2])))
FB_pure_win$bet <- NULL
FB_pure_win <- unique(FB_pure_win)
colnames(FB_pure_win) <- c("team", "wins")
FB_WL <- merge (FB_pure_win, FB_pure_lose, all = TRUE)
rm (FB_pure_lose)
rm (FB_pure_win)

#считаем долю побед FB, для тех, кто сыграл меньше 3 матчей - 0.5
FB_WL[is.na(FB_WL$wins),"wins"] <- 0
FB_WL[is.na(FB_WL$loses),"loses"] <- 0
temp <- FB_WL[,c(2,3)]
temp <- as.matrix(temp)
FB_WL$share <- apply (temp, 1, function(x) (x[1]/(x[1]+x[2])))
FB_WL$total <- apply (temp, 1, function(x) (x[1]+x[2]))
rm(temp)
FB_WL[FB_WL$total < 3,"share"] <- 0.5

#считаем вероятность выигрыша первой команды
FB.shares <- FB_WL$share
names(FB.shares) <- FB_WL$team
FB_probs <- outer(FB.shares, FB.shares, prob_FB)
diag(FB_probs) <- 0
FB_probs <- melt(FB_probs)

colnames(FB_probs)[1] <- "team1"
colnames(FB_probs)[2] <- "team2"
colnames(FB_probs)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- FB_probs[,c(3)]
temp <- as.matrix(temp)
FB_probs$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
FB_probs$coeff1 <- apply (temp, 1, function(x) 1/x[1])
FB_probs$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)



##################
#FB Bradley-Terry#
##################



#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]
FB_data <- FB_data[FB_data$loser %in% FB_data$winner, ]
FB_data <- FB_data[FB_data$winner %in% FB_data$loser, ]

#делаем фреймы для модели
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

rm(winner.frame)
rm(loser.frame)

FB.output <- data.frame(BTabilities(dotamatch.model))
FB.output <- FB.output[!is.na (FB.output$ability),]
FB.output$Pr <- apply (FB.output, 1, function(x) (x[1] /x[2]))
FB.output$Pr <- abs(FB.output$Pr)
FB.output <- FB.output[(FB.output$Pr>Pr & !is.na(FB.output$s.e.)),]
FB.output <- FB.output[(FB.output$Pr>Pr & !is.na(FB.output$s.e.)),]
team_names <- rownames(FB.output)
FB.abilities <- FB.output$ability
names(FB.abilities) <- team_names

FB_probs_BT <- outer(FB.abilities, FB.abilities, prob_BT)
diag(FB_probs_BT) <- 0
FB_probs_BT <- melt(FB_probs_BT)
colnames(FB_probs_BT)[1] <- "team1"
colnames(FB_probs_BT)[2] <- "team2"
colnames(FB_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- FB_probs_BT[,c(3)]
temp <- as.matrix(temp)
FB_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
FB_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
FB_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)


#####################
#Match Bradley-Terry#
#####################



#отбираем нужные данные по матчам
M_data <- EGB[EGB$bet_type == "GameResult",]
M_data_temp <- EGB[EGB$bet_type == "Total",]
M_data_temp <- M_data_temp[!(M_data_temp$series %in% M_data$series), ]
M_data <- merge(M_data, M_data_temp, all=TRUE)
M_data <- M_data[M_data$win != "draw" ,]
rm(M_data_temp)

M_data <- M_data[,c(1,2,3,4,5,6,7,8,9)]
M_data <- M_data[(M_data$date>Sys.Date()-time_frame),]

#прописываем явно победителя и проигравшего
M_data[M_data$win == "win1", "winner"] <- M_data[M_data$win == "win1", "team1"]
M_data[M_data$win == "win1", "loser"] <- M_data[M_data$win == "win1", "team2"]
M_data[M_data$win == "win2", "winner"] <- M_data[M_data$win == "win2", "team2"]
M_data[M_data$win == "win2", "loser"] <- M_data[M_data$win == "win2", "team1"]

#задаем уровни
levels(M_data[,"winner"]) <- unique(c(M_data[,"winner"], M_data[,"loser"]))
levels(M_data[,"loser"]) <- unique(c(M_data[,"winner"], M_data[,"loser"]))

#результат - везде побела winner'а
M_data$result <- rep (1, nrow (M_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
M_data <- M_data[M_data$loser %in% M_data$winner, ]
M_data <- M_data[M_data$winner %in% M_data$loser, ]
M_data <- M_data[M_data$loser %in% M_data$winner, ]
M_data <- M_data[M_data$winner %in% M_data$loser, ]
M_data <- M_data[M_data$loser %in% M_data$winner, ]
M_data <- M_data[M_data$winner %in% M_data$loser, ]
M_data <- M_data[M_data$loser %in% M_data$winner, ]
M_data <- M_data[M_data$winner %in% M_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(M_data$winner, M_data$bet))
loser.frame <- as.data.frame(cbind(M_data$loser, M_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = M_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

M.output <- data.frame(BTabilities(dotamatch.model))
M.output <- M.output[!is.na (M.output$ability),]
M.output$Pr <- apply (M.output, 1, function(x) (x[1] /x[2]))
M.output$Pr <- abs(M.output$Pr)
M.output <- M.output[(M.output$Pr>Pr & !is.na(M.output$s.e.)),]
M.output <- M.output[(M.output$Pr>Pr & !is.na(M.output$s.e.)),]
team_names <- rownames(M.output)
M.abilities <- M.output$ability
names(M.abilities) <- team_names

M_probs_BT <- outer(M.abilities, M.abilities, prob_BT)
diag(M_probs_BT) <- 0
M_probs_BT <- melt(M_probs_BT)
colnames(M_probs_BT)[1] <- "team1"
colnames(M_probs_BT)[2] <- "team2"
colnames(M_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- M_probs_BT[,c(3)]
temp <- as.matrix(temp)
M_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
M_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
M_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)


#####################
#Kills Bradley-Terry#
#####################



#отбираем нужные данные по матчам
K_data <- EGB[EGB$bet_type == "Kills10",]
K_data <- K_data[K_data$win != "draw" ,]

K_data <- K_data[,c(1,2,3,4,5,6,7,8,9)]
K_data <- K_data[(K_data$date>Sys.Date()-time_frame),]

#прописываем явно победителя и проигравшего
K_data[K_data$win == "win1", "winner"] <- K_data[K_data$win == "win1", "team1"]
K_data[K_data$win == "win1", "loser"] <- K_data[K_data$win == "win1", "team2"]
K_data[K_data$win == "win2", "winner"] <- K_data[K_data$win == "win2", "team2"]
K_data[K_data$win == "win2", "loser"] <- K_data[K_data$win == "win2", "team1"]

#задаем уровни
levels(K_data[,"winner"]) <- unique(c(K_data[,"winner"], K_data[,"loser"]))
levels(K_data[,"loser"]) <- unique(c(K_data[,"winner"], K_data[,"loser"]))

#результат - везде побела winner'а
K_data$result <- rep (1, nrow (K_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
K_data <- K_data[K_data$loser %in% K_data$winner, ]
K_data <- K_data[K_data$winner %in% K_data$loser, ]
K_data <- K_data[K_data$loser %in% K_data$winner, ]
K_data <- K_data[K_data$winner %in% K_data$loser, ]
K_data <- K_data[K_data$loser %in% K_data$winner, ]
K_data <- K_data[K_data$winner %in% K_data$loser, ]
K_data <- K_data[K_data$loser %in% K_data$winner, ]
K_data <- K_data[K_data$winner %in% K_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(K_data$winner, K_data$bet))
loser.frame <- as.data.frame(cbind(K_data$loser, K_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = K_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

K.output <- data.frame(BTabilities(dotamatch.model))
K.output <- K.output[!is.na (K.output$ability),]
K.output$Pr <- apply (K.output, 1, function(x) (x[1] /x[2]))
K.output$Pr <- abs(K.output$Pr)
K.output <- K.output[(K.output$Pr>Pr & !is.na(K.output$s.e.)),]
K.output <- K.output[(K.output$Pr>Pr & !is.na(K.output$s.e.)),]
team_names <- rownames(K.output)
K.abilities <- K.output$ability
names(K.abilities) <- team_names

K_probs_BT <- outer(K.abilities, K.abilities, prob_BT)
diag(K_probs_BT) <- 0
K_probs_BT <- melt(K_probs_BT)
colnames(K_probs_BT)[1] <- "team1"
colnames(K_probs_BT)[2] <- "team2"
colnames(K_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- K_probs_BT[,c(3)]
temp <- as.matrix(temp)
K_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
K_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
K_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)


########
#Result#
########


#выгрузка в DropBox
setwd("C:/Users/Привет/Dropbox")
write.csv(FB_WL, "FB_WL.csv")
write.csv(FB_probs, "FB_probs.csv")
write.csv(FB_probs_BT, "FB_probs_BT.csv")
write.csv(M_probs_BT, "M_probs_BT.csv")
write.csv(K_probs_BT, "K_probs_BT.csv")

#быстрая проверка для 2 команд по всем показателям
team1 = "Alternate"
team2 = "Pries"

FB_probs[FB_probs$team1 == team1 & FB_probs$team2 == team2,]
FB_probs_BT[FB_probs_BT$team1 == team1 & FB_probs_BT$team2 == team2,]
M_probs_BT[M_probs_BT$team1 == team1 & M_probs_BT$team2 == team2,]
K_probs_BT[K_probs_BT$team1 == team1 & K_probs_BT$team2 == team2,]


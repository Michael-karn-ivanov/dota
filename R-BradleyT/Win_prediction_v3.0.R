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

prob_FB <- function(D2_FB_1, D2_FB_2) {
  (D2_FB_1 + 1 - D2_FB_2)/2
}

Pr = 1.4 #граница отсечения отношения матожидания к стандартной ошибке, выше которой считаем результат верным
time_frame = 60*24*60*60 #временнАя граница анализа данных в секундах
bet_edge = 0.1 #граница разницы предсказания и коэффициента, выше которой ставим

#собираем данные из всех .csv за все дни
files = list.files(path = "D:/calculations/coeffgrabber New/Data", pattern="*.csv")
setwd("D:/calculations/coeffgrabber New/Data")
EGB <- read.csv("!EGB.csv", header = FALSE, stringsAsFactors=FALSE)

for (i in 1:(length(files)-1)) 
{
  EGB_temp <- read.csv(files[i], header = FALSE, stringsAsFactors=FALSE)
  EGB <- merge(EGB, EGB_temp, all=TRUE)
}

#строки без результата берем только из последнего файла, как самого актуального
EGB <- EGB[EGB$V12 != "draw",]
files
EGB_temp <- read.csv(files[length(files)], header = FALSE, stringsAsFactors=FALSE)
EGB <- merge(EGB, EGB_temp, all=TRUE)
rm(EGB_temp)

#немного чистим
EGB <- EGB[,c(2,1,11,3,6,7,12,9,4,5,8,10)]
colnames(EGB) <- c("game", "bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "map")
EGB$date <- as.POSIXct(EGB$date)
EGB <- EGB[(EGB$date>"2016/03/01"),]



########
#Dota 2#
########



D2 <- EGB[(EGB$game == 'Dota2'),c(2,3,4,5,6,7,8,9,10,11,12)]

#еще чистим
D2[D2$team1 == "!Rebels!","team1"] <- "Rebels"
D2[D2$team2 == "!Rebels!","team2"] <- "Rebels"
D2[D2$team1 == "BrooDMotherS","team1"] <- "BroodMothers"
D2[D2$team2 == "BrooDMotherS","team2"] <- "BroodMothers"
D2[D2$team1 == "The Mongolz","team1"] <- "TheMongolz"
D2[D2$team2 == "The Mongolz","team2"] <- "TheMongolz"



######################
#D2_Map Bradley-Terry#
######################



#отбираем нужные данные по матчам
D2_Map_data <- D2[D2$bet_type == "GameResult",]
D2_Map_data_temp <- D2[D2$bet_type == "Total",]
D2_Map_data_temp <- D2_Map_data_temp[!(D2_Map_data_temp$series %in% D2_Map_data$series), ]
D2_Map_data <- merge(D2_Map_data, D2_Map_data_temp, all=TRUE)
D2_future <- D2_Map_data[D2_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(D2_future)[4] <- "map_coeff1"
colnames(D2_future)[5] <- "map_coeff2"
D2_Map_data <- D2_Map_data[D2_Map_data$win != "draw" ,]
rm(D2_Map_data_temp)

D2_Map_data <- D2_Map_data[,c(1,2,3,4,5,6,7,8,9)]
D2_Map_data <- D2_Map_data[(D2_Map_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
D2_Map_data[D2_Map_data$win == "win1", "winner"] <- D2_Map_data[D2_Map_data$win == "win1", "team1"]
D2_Map_data[D2_Map_data$win == "win1", "loser"] <- D2_Map_data[D2_Map_data$win == "win1", "team2"]
D2_Map_data[D2_Map_data$win == "win2", "winner"] <- D2_Map_data[D2_Map_data$win == "win2", "team2"]
D2_Map_data[D2_Map_data$win == "win2", "loser"] <- D2_Map_data[D2_Map_data$win == "win2", "team1"]

#задаем уровни
levels(D2_Map_data[,"winner"]) <- unique(c(D2_Map_data[,"winner"], D2_Map_data[,"loser"]))
levels(D2_Map_data[,"loser"]) <- unique(c(D2_Map_data[,"winner"], D2_Map_data[,"loser"]))

#результат - везде побела winner'а
D2_Map_data$result <- rep (1, nrow (D2_Map_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
D2_Map_data <- D2_Map_data[D2_Map_data$loser %in% D2_Map_data$winner, ]
D2_Map_data <- D2_Map_data[D2_Map_data$winner %in% D2_Map_data$loser, ]
D2_Map_data <- D2_Map_data[D2_Map_data$loser %in% D2_Map_data$winner, ]
D2_Map_data <- D2_Map_data[D2_Map_data$winner %in% D2_Map_data$loser, ]
D2_Map_data <- D2_Map_data[D2_Map_data$loser %in% D2_Map_data$winner, ]
D2_Map_data <- D2_Map_data[D2_Map_data$winner %in% D2_Map_data$loser, ]
D2_Map_data <- D2_Map_data[D2_Map_data$loser %in% D2_Map_data$winner, ]
D2_Map_data <- D2_Map_data[D2_Map_data$winner %in% D2_Map_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(D2_Map_data$winner, D2_Map_data$bet))
loser.frame <- as.data.frame(cbind(D2_Map_data$loser, D2_Map_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = D2_Map_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

D2_Map.output <- data.frame(BTabilities(dotamatch.model))
D2_Map.output <- D2_Map.output[!is.na (D2_Map.output$ability),]
D2_Map.output$Pr <- apply (D2_Map.output, 1, function(x) (x[1] /x[2]))
D2_Map.output$Pr <- abs(D2_Map.output$Pr)
D2_Map.output <- D2_Map.output[(D2_Map.output$Pr>Pr & !is.na(D2_Map.output$s.e.)),]
D2_Map.output <- D2_Map.output[(D2_Map.output$Pr>Pr & !is.na(D2_Map.output$s.e.)),]
team_names <- rownames(D2_Map.output)
D2_Map.abilities <- D2_Map.output$ability
names(D2_Map.abilities) <- team_names

D2_Map_probs_BT <- outer(D2_Map.abilities, D2_Map.abilities, prob_BT)
diag(D2_Map_probs_BT) <- 0
D2_Map_probs_BT <- melt(D2_Map_probs_BT)
colnames(D2_Map_probs_BT)[1] <- "team1"
colnames(D2_Map_probs_BT)[2] <- "team2"
colnames(D2_Map_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- D2_Map_probs_BT[,c(3)]
temp <- as.matrix(temp)
D2_Map_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
D2_Map_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
D2_Map_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(D2_Map.output)



##########################
#D2_FB Bradley-Terry + WL#
##########################



#отбираем нужные данные по D2_FB
D2_FB_data <- D2[grep("lood", D2$bet_type),]
D2_FB_data_temp <- D2[D2$bet_type == "FB",]
D2_FB_data <- merge(D2_FB_data, D2_FB_data_temp, all=TRUE)
rm(D2_FB_data_temp)
D2_FB_data_temp <- D2_FB_data[D2_FB_data$win == "draw", c(3,4,5,8,9)]
D2_future <- merge (D2_future,D2_FB_data_temp,all.x = TRUE)
colnames(D2_future)[6] <- "fb_coeff1"
colnames(D2_future)[7] <- "fb_coeff2"
D2_FB_data <- D2_FB_data[D2_FB_data$win != "draw" ,]
rm(D2_FB_data_temp)

D2_FB_data <- D2_FB_data[,c(1,2,3,4,5,6,7,8,9)]
D2_FB_data <- D2_FB_data[(D2_FB_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
D2_FB_data[D2_FB_data$win == "win1", "winner"] <- D2_FB_data[D2_FB_data$win == "win1", "team1"]
D2_FB_data[D2_FB_data$win == "win1", "loser"] <- D2_FB_data[D2_FB_data$win == "win1", "team2"]
D2_FB_data[D2_FB_data$win == "win2", "winner"] <- D2_FB_data[D2_FB_data$win == "win2", "team2"]
D2_FB_data[D2_FB_data$win == "win2", "loser"] <- D2_FB_data[D2_FB_data$win == "win2", "team1"]

#задаем уровни
levels(D2_FB_data[,"winner"]) <- unique(c(D2_FB_data[,"winner"], D2_FB_data[,"loser"]))
levels(D2_FB_data[,"loser"]) <- unique(c(D2_FB_data[,"winner"], D2_FB_data[,"loser"]))

#результат - везде побела winner'а
D2_FB_data$result <- rep (1, nrow (D2_FB_data))

#делаем таблицу с кол-вом выигранных и проигранных D2_FB для каждой команды
D2_FB_pure_lose <- D2_FB_data[, c(1,11)]
D2_FB_pure_lose$num <- apply (D2_FB_pure_lose, 1, function(x) (sum(D2_FB_pure_lose$loser == x[2])))
D2_FB_pure_lose$bet <- NULL
D2_FB_pure_lose <- unique(D2_FB_pure_lose)
colnames(D2_FB_pure_lose) <- c("team", "loses")
D2_FB_pure_win <- D2_FB_data[, c(1,10)]
D2_FB_pure_win$num <- apply (D2_FB_pure_win, 1, function(x) (sum(D2_FB_pure_win$winner == x[2])))
D2_FB_pure_win$bet <- NULL
D2_FB_pure_win <- unique(D2_FB_pure_win)
colnames(D2_FB_pure_win) <- c("team", "wins")
D2_FB_WL <- merge (D2_FB_pure_win, D2_FB_pure_lose, all = TRUE)
rm (D2_FB_pure_lose)
rm (D2_FB_pure_win)

#считаем долю побед D2_FB, для тех, кто сыграл меньше 3 матчей - 0.5
D2_FB_WL[is.na(D2_FB_WL$wins),"wins"] <- 0
D2_FB_WL[is.na(D2_FB_WL$loses),"loses"] <- 0
temp <- D2_FB_WL[,c(2,3)]
temp <- as.matrix(temp)
D2_FB_WL$share <- apply (temp, 1, function(x) (x[1]/(x[1]+x[2])))
D2_FB_WL$total <- apply (temp, 1, function(x) (x[1]+x[2]))
rm(temp)
D2_FB_WL[D2_FB_WL$total < 3,"share"] <- 0.5

#считаем вероятность выигрыша первой команды
D2_FB.shares <- D2_FB_WL$share
names(D2_FB.shares) <- D2_FB_WL$team
D2_FB_probs <- outer(D2_FB.shares, D2_FB.shares, prob_FB)
diag(D2_FB_probs) <- 0
D2_FB_probs <- melt(D2_FB_probs)

colnames(D2_FB_probs)[1] <- "team1"
colnames(D2_FB_probs)[2] <- "team2"
colnames(D2_FB_probs)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- D2_FB_probs[,c(3)]
temp <- as.matrix(temp)
D2_FB_probs$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
D2_FB_probs$coeff1 <- apply (temp, 1, function(x) 1/x[1])
D2_FB_probs$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
D2_FB_data <- D2_FB_data[D2_FB_data$loser %in% D2_FB_data$winner, ]
D2_FB_data <- D2_FB_data[D2_FB_data$winner %in% D2_FB_data$loser, ]
D2_FB_data <- D2_FB_data[D2_FB_data$loser %in% D2_FB_data$winner, ]
D2_FB_data <- D2_FB_data[D2_FB_data$winner %in% D2_FB_data$loser, ]
D2_FB_data <- D2_FB_data[D2_FB_data$loser %in% D2_FB_data$winner, ]
D2_FB_data <- D2_FB_data[D2_FB_data$winner %in% D2_FB_data$loser, ]
D2_FB_data <- D2_FB_data[D2_FB_data$loser %in% D2_FB_data$winner, ]
D2_FB_data <- D2_FB_data[D2_FB_data$winner %in% D2_FB_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(D2_FB_data$winner, D2_FB_data$bet))
loser.frame <- as.data.frame(cbind(D2_FB_data$loser, D2_FB_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = D2_FB_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

D2_FB.output <- data.frame(BTabilities(dotamatch.model))
D2_FB.output <- D2_FB.output[!is.na (D2_FB.output$ability),]
D2_FB.output$Pr <- apply (D2_FB.output, 1, function(x) (x[1] /x[2]))
D2_FB.output$Pr <- abs(D2_FB.output$Pr)
D2_FB.output <- D2_FB.output[(D2_FB.output$Pr>Pr & !is.na(D2_FB.output$s.e.)),]
D2_FB.output <- D2_FB.output[(D2_FB.output$Pr>Pr & !is.na(D2_FB.output$s.e.)),]
team_names <- rownames(D2_FB.output)
D2_FB.abilities <- D2_FB.output$ability
names(D2_FB.abilities) <- team_names

D2_FB_probs_BT <- outer(D2_FB.abilities, D2_FB.abilities, prob_BT)
diag(D2_FB_probs_BT) <- 0
D2_FB_probs_BT <- melt(D2_FB_probs_BT)
colnames(D2_FB_probs_BT)[1] <- "team1"
colnames(D2_FB_probs_BT)[2] <- "team2"
colnames(D2_FB_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- D2_FB_probs_BT[,c(3)]
temp <- as.matrix(temp)
D2_FB_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
D2_FB_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
D2_FB_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(D2_FB.output)



#####################
#Kills Bradley-Terry#
#####################



#отбираем нужные данные по матчам
D2_Kills_data <- D2[D2$bet_type == "Kills10",]
D2_Kills_data_temp <- D2_Kills_data[D2_Kills_data$win == "draw", c(3,4,5,8,9)]
D2_future <- merge (D2_future,D2_Kills_data_temp,all.x = TRUE)
colnames(D2_future)[8] <- "kills_coeff1"
colnames(D2_future)[9] <- "kills_coeff2"
rm(D2_Kills_data_temp)
D2_Kills_data <- D2_Kills_data[D2_Kills_data$win != "draw" ,]

D2_Kills_data <- D2_Kills_data[,c(1,2,3,4,5,6,7,8,9)]
D2_Kills_data <- D2_Kills_data[(D2_Kills_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
D2_Kills_data[D2_Kills_data$win == "win1", "winner"] <- D2_Kills_data[D2_Kills_data$win == "win1", "team1"]
D2_Kills_data[D2_Kills_data$win == "win1", "loser"] <- D2_Kills_data[D2_Kills_data$win == "win1", "team2"]
D2_Kills_data[D2_Kills_data$win == "win2", "winner"] <- D2_Kills_data[D2_Kills_data$win == "win2", "team2"]
D2_Kills_data[D2_Kills_data$win == "win2", "loser"] <- D2_Kills_data[D2_Kills_data$win == "win2", "team1"]

#задаем уровни
levels(D2_Kills_data[,"winner"]) <- unique(c(D2_Kills_data[,"winner"], D2_Kills_data[,"loser"]))
levels(D2_Kills_data[,"loser"]) <- unique(c(D2_Kills_data[,"winner"], D2_Kills_data[,"loser"]))

#результат - везде побела winner'а
D2_Kills_data$result <- rep (1, nrow (D2_Kills_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
D2_Kills_data <- D2_Kills_data[D2_Kills_data$loser %in% D2_Kills_data$winner, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$winner %in% D2_Kills_data$loser, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$loser %in% D2_Kills_data$winner, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$winner %in% D2_Kills_data$loser, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$loser %in% D2_Kills_data$winner, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$winner %in% D2_Kills_data$loser, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$loser %in% D2_Kills_data$winner, ]
D2_Kills_data <- D2_Kills_data[D2_Kills_data$winner %in% D2_Kills_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(D2_Kills_data$winner, D2_Kills_data$bet))
loser.frame <- as.data.frame(cbind(D2_Kills_data$loser, D2_Kills_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = D2_Kills_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

D2_Kills.output <- data.frame(BTabilities(dotamatch.model))
D2_Kills.output <- D2_Kills.output[!is.na (D2_Kills.output$ability),]
D2_Kills.output$Pr <- apply (D2_Kills.output, 1, function(x) (x[1] /x[2]))
D2_Kills.output$Pr <- abs(D2_Kills.output$Pr)
D2_Kills.output <- D2_Kills.output[(D2_Kills.output$Pr>Pr & !is.na(D2_Kills.output$s.e.)),]
D2_Kills.output <- D2_Kills.output[(D2_Kills.output$Pr>Pr & !is.na(D2_Kills.output$s.e.)),]
team_names <- rownames(D2_Kills.output)
D2_Kills.abilities <- D2_Kills.output$ability
names(D2_Kills.abilities) <- team_names

D2_Kills_probs_BT <- outer(D2_Kills.abilities, D2_Kills.abilities, prob_BT)
diag(D2_Kills_probs_BT) <- 0
D2_Kills_probs_BT <- melt(D2_Kills_probs_BT)
colnames(D2_Kills_probs_BT)[1] <- "team1"
colnames(D2_Kills_probs_BT)[2] <- "team2"
colnames(D2_Kills_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- D2_Kills_probs_BT[,c(3)]
temp <- as.matrix(temp)
D2_Kills_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
D2_Kills_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
D2_Kills_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(D2_Kills.output)

########
#Result#
########

if (length(D2_Map_probs_BT) >0){
D2_future <- merge (D2_future, D2_Map_probs_BT,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[10] <- "map_pred1"
colnames(D2_future)[11] <- "map_pred2"
} else {
  D2_future$map_pred1 <- NA
  D2_future$map_pred2 <- NA
}

if (length(D2_FB_probs_BT) >0){
D2_future <- merge (D2_future, D2_FB_probs_BT,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[12] <- "fb_pred1"
colnames(D2_future)[13] <- "fb_pred2"
} else {
  D2_future$fb_pred1 <- NA
  D2_future$fb_pred2 <- NA
}


D2_future <- merge (D2_future, D2_FB_probs,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[14] <- "fb_wl_pred1"
colnames(D2_future)[15] <- "fb_wl_pred2"

if (length(D2_Kills_probs_BT) >0){
D2_future <- merge (D2_future, D2_Kills_probs_BT,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[16] <- "kills_pred1"
colnames(D2_future)[17] <- "kills_pred2"
} else {
  D2_future$kills_pred1 <- NA
  D2_future$kills_pred2 <- NA
}

D2_future[is.na(D2_future$map_coeff1),"map_coeff1"] <- 0
D2_future[is.na(D2_future$map_coeff2),"map_coeff2"] <- 0
D2_future[is.na(D2_future$fb_coeff1),"fb_coeff1"] <- 0
D2_future[is.na(D2_future$fb_coeff2),"fb_coeff2"] <- 0
D2_future[is.na(D2_future$kills_coeff1),"kills_coeff1"] <- 0
D2_future[is.na(D2_future$kills_coeff2),"kills_coeff2"] <- 0
D2_future[is.na(D2_future$map_pred1),"map_pred1"] <- 100
D2_future[is.na(D2_future$map_pred2),"map_pred2"] <- 100
D2_future[is.na(D2_future$fb_pred1),"fb_pred1"] <- 100
D2_future[is.na(D2_future$fb_pred2),"fb_pred2"] <- 100
D2_future[is.na(D2_future$fb_wl_pred1),"fb_wl_pred1"] <- 100
D2_future[is.na(D2_future$fb_wl_pred2),"fb_wl_pred2"] <- 100
D2_future[is.na(D2_future$kills_pred1),"kills_pred1"] <- 100
D2_future[is.na(D2_future$kills_pred2),"kills_pred2"] <- 100

D2_Result <- D2_future
D2_Result$map <- NA
D2_Result$fb <- NA
D2_Result$kills <- NA
D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "team1"]
D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "team2"]
D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "team1"]
D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "team2"]

D2_Result$game <- "Dota"
D2_Result <- D2_Result[,c(21,3,1,2,18,19,20)]
D2_Result <- D2_Result[(!is.na(D2_Result$map) | !is.na(D2_Result$fb) | !is.na(D2_Result$kills)),]



########
##LoL###
########



LoL <- EGB[(EGB$game == 'LoL'),c(2,3,4,5,6,7,8,9,10,11,12)]



######################
#LoL_Map Bradley-Terry#
######################



#отбираем нужные данные по матчам
LoL_Map_data <- LoL[LoL$bet_type == "GameResult",]
LoL_Map_data_temp <- LoL[LoL$bet_type == "Total",]
LoL_Map_data_temp <- LoL_Map_data_temp[!(LoL_Map_data_temp$series %in% LoL_Map_data$series), ]
LoL_Map_data <- merge(LoL_Map_data, LoL_Map_data_temp, all=TRUE)
LoL_future <- LoL_Map_data[LoL_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(LoL_future)[4] <- "map_coeff1"
colnames(LoL_future)[5] <- "map_coeff2"
LoL_Map_data <- LoL_Map_data[LoL_Map_data$win != "draw" ,]
rm(LoL_Map_data_temp)

LoL_Map_data <- LoL_Map_data[,c(1,2,3,4,5,6,7,8,9)]
LoL_Map_data <- LoL_Map_data[(LoL_Map_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
LoL_Map_data[LoL_Map_data$win == "win1", "winner"] <- LoL_Map_data[LoL_Map_data$win == "win1", "team1"]
LoL_Map_data[LoL_Map_data$win == "win1", "loser"] <- LoL_Map_data[LoL_Map_data$win == "win1", "team2"]
LoL_Map_data[LoL_Map_data$win == "win2", "winner"] <- LoL_Map_data[LoL_Map_data$win == "win2", "team2"]
LoL_Map_data[LoL_Map_data$win == "win2", "loser"] <- LoL_Map_data[LoL_Map_data$win == "win2", "team1"]

#задаем уровни
levels(LoL_Map_data[,"winner"]) <- unique(c(LoL_Map_data[,"winner"], LoL_Map_data[,"loser"]))
levels(LoL_Map_data[,"loser"]) <- unique(c(LoL_Map_data[,"winner"], LoL_Map_data[,"loser"]))

#результат - везде побела winner'а
LoL_Map_data$result <- rep (1, nrow (LoL_Map_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
LoL_Map_data <- LoL_Map_data[LoL_Map_data$loser %in% LoL_Map_data$winner, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$winner %in% LoL_Map_data$loser, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$loser %in% LoL_Map_data$winner, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$winner %in% LoL_Map_data$loser, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$loser %in% LoL_Map_data$winner, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$winner %in% LoL_Map_data$loser, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$loser %in% LoL_Map_data$winner, ]
LoL_Map_data <- LoL_Map_data[LoL_Map_data$winner %in% LoL_Map_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(LoL_Map_data$winner, LoL_Map_data$bet))
loser.frame <- as.data.frame(cbind(LoL_Map_data$loser, LoL_Map_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = LoL_Map_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

LoL_Map.output <- data.frame(BTabilities(dotamatch.model))
LoL_Map.output <- LoL_Map.output[!is.na (LoL_Map.output$ability),]
LoL_Map.output$Pr <- apply (LoL_Map.output, 1, function(x) (x[1] /x[2]))
LoL_Map.output$Pr <- abs(LoL_Map.output$Pr)
LoL_Map.output <- LoL_Map.output[(LoL_Map.output$Pr>Pr & !is.na(LoL_Map.output$s.e.)),]
LoL_Map.output <- LoL_Map.output[(LoL_Map.output$Pr>Pr & !is.na(LoL_Map.output$s.e.)),]
team_names <- rownames(LoL_Map.output)
LoL_Map.abilities <- LoL_Map.output$ability
names(LoL_Map.abilities) <- team_names

LoL_Map_probs_BT <- outer(LoL_Map.abilities, LoL_Map.abilities, prob_BT)
diag(LoL_Map_probs_BT) <- 0
LoL_Map_probs_BT <- melt(LoL_Map_probs_BT)
colnames(LoL_Map_probs_BT)[1] <- "team1"
colnames(LoL_Map_probs_BT)[2] <- "team2"
colnames(LoL_Map_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- LoL_Map_probs_BT[,c(3)]
temp <- as.matrix(temp)
LoL_Map_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
LoL_Map_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
LoL_Map_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(LoL_Map.output)



##########################
#LoL_FB Bradley-Terry + WL#
##########################



#отбираем нужные данные по LoL_FB
LoL_FB_data <- LoL[grep("lood", LoL$bet_type),]
LoL_FB_data_temp <- LoL[LoL$bet_type == "FB",]
LoL_FB_data <- merge(LoL_FB_data, LoL_FB_data_temp, all=TRUE)
rm(LoL_FB_data_temp)
LoL_FB_data_temp <- LoL_FB_data[LoL_FB_data$win == "draw", c(3,4,5,8,9)]
LoL_future <- merge (LoL_future,LoL_FB_data_temp,all.x = TRUE)
colnames(LoL_future)[6] <- "fb_coeff1"
colnames(LoL_future)[7] <- "fb_coeff2"
LoL_FB_data <- LoL_FB_data[LoL_FB_data$win != "draw" ,]
rm(LoL_FB_data_temp)

LoL_FB_data <- LoL_FB_data[,c(1,2,3,4,5,6,7,8,9)]
LoL_FB_data <- LoL_FB_data[(LoL_FB_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
LoL_FB_data[LoL_FB_data$win == "win1", "winner"] <- LoL_FB_data[LoL_FB_data$win == "win1", "team1"]
LoL_FB_data[LoL_FB_data$win == "win1", "loser"] <- LoL_FB_data[LoL_FB_data$win == "win1", "team2"]
LoL_FB_data[LoL_FB_data$win == "win2", "winner"] <- LoL_FB_data[LoL_FB_data$win == "win2", "team2"]
LoL_FB_data[LoL_FB_data$win == "win2", "loser"] <- LoL_FB_data[LoL_FB_data$win == "win2", "team1"]

#задаем уровни
levels(LoL_FB_data[,"winner"]) <- unique(c(LoL_FB_data[,"winner"], LoL_FB_data[,"loser"]))
levels(LoL_FB_data[,"loser"]) <- unique(c(LoL_FB_data[,"winner"], LoL_FB_data[,"loser"]))

#результат - везде побела winner'а
LoL_FB_data$result <- rep (1, nrow (LoL_FB_data))

#делаем таблицу с кол-вом выигранных и проигранных LoL_FB для каждой команды
LoL_FB_pure_lose <- LoL_FB_data[, c(1,11)]
LoL_FB_pure_lose$num <- apply (LoL_FB_pure_lose, 1, function(x) (sum(LoL_FB_pure_lose$loser == x[2])))
LoL_FB_pure_lose$bet <- NULL
LoL_FB_pure_lose <- unique(LoL_FB_pure_lose)
colnames(LoL_FB_pure_lose) <- c("team", "loses")
LoL_FB_pure_win <- LoL_FB_data[, c(1,10)]
LoL_FB_pure_win$num <- apply (LoL_FB_pure_win, 1, function(x) (sum(LoL_FB_pure_win$winner == x[2])))
LoL_FB_pure_win$bet <- NULL
LoL_FB_pure_win <- unique(LoL_FB_pure_win)
colnames(LoL_FB_pure_win) <- c("team", "wins")
LoL_FB_WL <- merge (LoL_FB_pure_win, LoL_FB_pure_lose, all = TRUE)
rm (LoL_FB_pure_lose)
rm (LoL_FB_pure_win)

#считаем долю побед LoL_FB, для тех, кто сыграл меньше 3 матчей - 0.5
LoL_FB_WL[is.na(LoL_FB_WL$wins),"wins"] <- 0
LoL_FB_WL[is.na(LoL_FB_WL$loses),"loses"] <- 0
temp <- LoL_FB_WL[,c(2,3)]
temp <- as.matrix(temp)
LoL_FB_WL$share <- apply (temp, 1, function(x) (x[1]/(x[1]+x[2])))
LoL_FB_WL$total <- apply (temp, 1, function(x) (x[1]+x[2]))
rm(temp)
LoL_FB_WL[LoL_FB_WL$total < 3,"share"] <- 0.5

#считаем вероятность выигрыша первой команды
LoL_FB.shares <- LoL_FB_WL$share
names(LoL_FB.shares) <- LoL_FB_WL$team
LoL_FB_probs <- outer(LoL_FB.shares, LoL_FB.shares, prob_FB)
diag(LoL_FB_probs) <- 0
LoL_FB_probs <- melt(LoL_FB_probs)

colnames(LoL_FB_probs)[1] <- "team1"
colnames(LoL_FB_probs)[2] <- "team2"
colnames(LoL_FB_probs)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- LoL_FB_probs[,c(3)]
temp <- as.matrix(temp)
LoL_FB_probs$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
LoL_FB_probs$coeff1 <- apply (temp, 1, function(x) 1/x[1])
LoL_FB_probs$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
LoL_FB_data <- LoL_FB_data[LoL_FB_data$loser %in% LoL_FB_data$winner, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$winner %in% LoL_FB_data$loser, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$loser %in% LoL_FB_data$winner, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$winner %in% LoL_FB_data$loser, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$loser %in% LoL_FB_data$winner, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$winner %in% LoL_FB_data$loser, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$loser %in% LoL_FB_data$winner, ]
LoL_FB_data <- LoL_FB_data[LoL_FB_data$winner %in% LoL_FB_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(LoL_FB_data$winner, LoL_FB_data$bet))
loser.frame <- as.data.frame(cbind(LoL_FB_data$loser, LoL_FB_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = LoL_FB_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

LoL_FB.output <- data.frame(BTabilities(dotamatch.model))
LoL_FB.output <- LoL_FB.output[!is.na (LoL_FB.output$ability),]
LoL_FB.output$Pr <- apply (LoL_FB.output, 1, function(x) (x[1] /x[2]))
LoL_FB.output$Pr <- abs(LoL_FB.output$Pr)
LoL_FB.output <- LoL_FB.output[(LoL_FB.output$Pr>Pr & !is.na(LoL_FB.output$s.e.)),]
LoL_FB.output <- LoL_FB.output[(LoL_FB.output$Pr>Pr & !is.na(LoL_FB.output$s.e.)),]
team_names <- rownames(LoL_FB.output)
LoL_FB.abilities <- LoL_FB.output$ability
names(LoL_FB.abilities) <- team_names

LoL_FB_probs_BT <- outer(LoL_FB.abilities, LoL_FB.abilities, prob_BT)
diag(LoL_FB_probs_BT) <- 0
LoL_FB_probs_BT <- melt(LoL_FB_probs_BT)
colnames(LoL_FB_probs_BT)[1] <- "team1"
colnames(LoL_FB_probs_BT)[2] <- "team2"
colnames(LoL_FB_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- LoL_FB_probs_BT[,c(3)]
temp <- as.matrix(temp)
LoL_FB_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
LoL_FB_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
LoL_FB_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(LoL_FB.output)



#####################
#Kills Bradley-Terry#
#####################



#отбираем нужные данные по матчам
LoL_Kills_data <- LoL[LoL$bet_type == "Kills10",]
LoL_Kills_data_temp <- LoL_Kills_data[LoL_Kills_data$win == "draw", c(3,4,5,8,9)]
LoL_future <- merge (LoL_future,LoL_Kills_data_temp,all.x = TRUE)
colnames(LoL_future)[8] <- "kills_coeff1"
colnames(LoL_future)[9] <- "kills_coeff2"
rm(LoL_Kills_data_temp)
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$win != "draw" ,]

LoL_Kills_data <- LoL_Kills_data[,c(1,2,3,4,5,6,7,8,9)]
LoL_Kills_data <- LoL_Kills_data[(LoL_Kills_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
LoL_Kills_data[LoL_Kills_data$win == "win1", "winner"] <- LoL_Kills_data[LoL_Kills_data$win == "win1", "team1"]
LoL_Kills_data[LoL_Kills_data$win == "win1", "loser"] <- LoL_Kills_data[LoL_Kills_data$win == "win1", "team2"]
LoL_Kills_data[LoL_Kills_data$win == "win2", "winner"] <- LoL_Kills_data[LoL_Kills_data$win == "win2", "team2"]
LoL_Kills_data[LoL_Kills_data$win == "win2", "loser"] <- LoL_Kills_data[LoL_Kills_data$win == "win2", "team1"]

#задаем уровни
levels(LoL_Kills_data[,"winner"]) <- unique(c(LoL_Kills_data[,"winner"], LoL_Kills_data[,"loser"]))
levels(LoL_Kills_data[,"loser"]) <- unique(c(LoL_Kills_data[,"winner"], LoL_Kills_data[,"loser"]))

#результат - везде побела winner'а
LoL_Kills_data$result <- rep (1, nrow (LoL_Kills_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$loser %in% LoL_Kills_data$winner, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$winner %in% LoL_Kills_data$loser, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$loser %in% LoL_Kills_data$winner, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$winner %in% LoL_Kills_data$loser, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$loser %in% LoL_Kills_data$winner, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$winner %in% LoL_Kills_data$loser, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$loser %in% LoL_Kills_data$winner, ]
LoL_Kills_data <- LoL_Kills_data[LoL_Kills_data$winner %in% LoL_Kills_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(LoL_Kills_data$winner, LoL_Kills_data$bet))
loser.frame <- as.data.frame(cbind(LoL_Kills_data$loser, LoL_Kills_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = LoL_Kills_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

LoL_Kills.output <- data.frame(BTabilities(dotamatch.model))
LoL_Kills.output <- LoL_Kills.output[!is.na (LoL_Kills.output$ability),]
LoL_Kills.output$Pr <- apply (LoL_Kills.output, 1, function(x) (x[1] /x[2]))
LoL_Kills.output$Pr <- abs(LoL_Kills.output$Pr)
LoL_Kills.output <- LoL_Kills.output[(LoL_Kills.output$Pr>Pr & !is.na(LoL_Kills.output$s.e.)),]
LoL_Kills.output <- LoL_Kills.output[(LoL_Kills.output$Pr>Pr & !is.na(LoL_Kills.output$s.e.)),]
team_names <- rownames(LoL_Kills.output)
LoL_Kills.abilities <- LoL_Kills.output$ability
names(LoL_Kills.abilities) <- team_names

LoL_Kills_probs_BT <- outer(LoL_Kills.abilities, LoL_Kills.abilities, prob_BT)
diag(LoL_Kills_probs_BT) <- 0
LoL_Kills_probs_BT <- melt(LoL_Kills_probs_BT)
colnames(LoL_Kills_probs_BT)[1] <- "team1"
colnames(LoL_Kills_probs_BT)[2] <- "team2"
colnames(LoL_Kills_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- LoL_Kills_probs_BT[,c(3)]
temp <- as.matrix(temp)
LoL_Kills_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
LoL_Kills_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
LoL_Kills_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(LoL_Kills.output)

########
#Result#
########

if (length(LoL_Map_probs_BT) >0){
  LoL_future <- merge (LoL_future, LoL_Map_probs_BT,all.x=TRUE)
  LoL_future$Pr1 <- NULL
  LoL_future$Pr2 <- NULL
  colnames(LoL_future)[10] <- "map_pred1"
  colnames(LoL_future)[11] <- "map_pred2"
} else {
  LoL_future$map_pred1 <- NA
  LoL_future$map_pred2 <- NA
}

if (length(LoL_FB_probs_BT) >0){
  LoL_future <- merge (LoL_future, LoL_FB_probs_BT,all.x=TRUE)
  LoL_future$Pr1 <- NULL
  LoL_future$Pr2 <- NULL
  colnames(LoL_future)[12] <- "fb_pred1"
  colnames(LoL_future)[13] <- "fb_pred2"
} else {
  LoL_future$fb_pred1 <- NA
  LoL_future$fb_pred2 <- NA
}


LoL_future <- merge (LoL_future, LoL_FB_probs,all.x=TRUE)
LoL_future$Pr1 <- NULL
LoL_future$Pr2 <- NULL
colnames(LoL_future)[14] <- "fb_wl_pred1"
colnames(LoL_future)[15] <- "fb_wl_pred2"

if (length(LoL_Kills_probs_BT) >0){
  LoL_future <- merge (LoL_future, LoL_Kills_probs_BT,all.x=TRUE)
  LoL_future$Pr1 <- NULL
  LoL_future$Pr2 <- NULL
  colnames(LoL_future)[16] <- "kills_pred1"
  colnames(LoL_future)[17] <- "kills_pred2"
} else {
  LoL_future$kills_pred1 <- NA
  LoL_future$kills_pred2 <- NA
}

LoL_future[is.na(LoL_future$map_coeff1),"map_coeff1"] <- 0
LoL_future[is.na(LoL_future$map_coeff2),"map_coeff2"] <- 0
LoL_future[is.na(LoL_future$fb_coeff1),"fb_coeff1"] <- 0
LoL_future[is.na(LoL_future$fb_coeff2),"fb_coeff2"] <- 0
LoL_future[is.na(LoL_future$kills_coeff1),"kills_coeff1"] <- 0
LoL_future[is.na(LoL_future$kills_coeff2),"kills_coeff2"] <- 0
LoL_future[is.na(LoL_future$map_pred1),"map_pred1"] <- 100
LoL_future[is.na(LoL_future$map_pred2),"map_pred2"] <- 100
LoL_future[is.na(LoL_future$fb_pred1),"fb_pred1"] <- 100
LoL_future[is.na(LoL_future$fb_pred2),"fb_pred2"] <- 100
LoL_future[is.na(LoL_future$fb_wl_pred1),"fb_wl_pred1"] <- 100
LoL_future[is.na(LoL_future$fb_wl_pred2),"fb_wl_pred2"] <- 100
LoL_future[is.na(LoL_future$kills_pred1),"kills_pred1"] <- 100
LoL_future[is.na(LoL_future$kills_pred2),"kills_pred2"] <- 100

LoL_Result <- LoL_future
LoL_Result$map <- NA
LoL_Result$fb <- NA
LoL_Result$kills <- NA
LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "team2"]

LoL_Result$game <- "LoL"
LoL_Result <- LoL_Result[,c(21,3,1,2,18,19,20)]
LoL_Result <- LoL_Result[(!is.na(LoL_Result$map) | !is.na(LoL_Result$fb) | !is.na(LoL_Result$kills)),]

Result <- merge(D2_Result, LoL_Result, all = TRUE)



########
#CS:GO##
########



CS <- EGB[(EGB$game == 'Counter-Strike'),c(2,3,4,5,6,7,8,9,10,11,12)]



######################
#CS_Map Bradley-Terry#
######################



#отбираем нужные данные по матчам
CS_Map_data <- CS[CS$bet_type == "GameResult",]
CS_Map_data_temp <- CS[CS$bet_type == "Total",]
CS_Map_data_temp <- CS_Map_data_temp[!(CS_Map_data_temp$series %in% CS_Map_data$series), ]
CS_Map_data <- merge(CS_Map_data, CS_Map_data_temp, all=TRUE)
CS_future <- CS_Map_data[CS_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(CS_future)[4] <- "map_coeff1"
colnames(CS_future)[5] <- "map_coeff2"
CS_Map_data <- CS_Map_data[CS_Map_data$win != "draw" ,]
rm(CS_Map_data_temp)

CS_Map_data <- CS_Map_data[,c(1,2,3,4,5,6,7,8,9)]
CS_Map_data <- CS_Map_data[(CS_Map_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
CS_Map_data[CS_Map_data$win == "win1", "winner"] <- CS_Map_data[CS_Map_data$win == "win1", "team1"]
CS_Map_data[CS_Map_data$win == "win1", "loser"] <- CS_Map_data[CS_Map_data$win == "win1", "team2"]
CS_Map_data[CS_Map_data$win == "win2", "winner"] <- CS_Map_data[CS_Map_data$win == "win2", "team2"]
CS_Map_data[CS_Map_data$win == "win2", "loser"] <- CS_Map_data[CS_Map_data$win == "win2", "team1"]

#задаем уровни
levels(CS_Map_data[,"winner"]) <- unique(c(CS_Map_data[,"winner"], CS_Map_data[,"loser"]))
levels(CS_Map_data[,"loser"]) <- unique(c(CS_Map_data[,"winner"], CS_Map_data[,"loser"]))

#результат - везде побела winner'а
CS_Map_data$result <- rep (1, nrow (CS_Map_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
CS_Map_data <- CS_Map_data[CS_Map_data$loser %in% CS_Map_data$winner, ]
CS_Map_data <- CS_Map_data[CS_Map_data$winner %in% CS_Map_data$loser, ]
CS_Map_data <- CS_Map_data[CS_Map_data$loser %in% CS_Map_data$winner, ]
CS_Map_data <- CS_Map_data[CS_Map_data$winner %in% CS_Map_data$loser, ]
CS_Map_data <- CS_Map_data[CS_Map_data$loser %in% CS_Map_data$winner, ]
CS_Map_data <- CS_Map_data[CS_Map_data$winner %in% CS_Map_data$loser, ]
CS_Map_data <- CS_Map_data[CS_Map_data$loser %in% CS_Map_data$winner, ]
CS_Map_data <- CS_Map_data[CS_Map_data$winner %in% CS_Map_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(CS_Map_data$winner, CS_Map_data$bet))
loser.frame <- as.data.frame(cbind(CS_Map_data$loser, CS_Map_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = CS_Map_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

CS_Map.output <- data.frame(BTabilities(dotamatch.model))
CS_Map.output <- CS_Map.output[!is.na (CS_Map.output$ability),]
CS_Map.output$Pr <- apply (CS_Map.output, 1, function(x) (x[1] /x[2]))
CS_Map.output$Pr <- abs(CS_Map.output$Pr)
CS_Map.output <- CS_Map.output[(CS_Map.output$Pr>Pr & !is.na(CS_Map.output$s.e.)),]
CS_Map.output <- CS_Map.output[(CS_Map.output$Pr>Pr & !is.na(CS_Map.output$s.e.)),]
team_names <- rownames(CS_Map.output)
CS_Map.abilities <- CS_Map.output$ability
names(CS_Map.abilities) <- team_names

CS_Map_probs_BT <- outer(CS_Map.abilities, CS_Map.abilities, prob_BT)
diag(CS_Map_probs_BT) <- 0
CS_Map_probs_BT <- melt(CS_Map_probs_BT)
colnames(CS_Map_probs_BT)[1] <- "team1"
colnames(CS_Map_probs_BT)[2] <- "team2"
colnames(CS_Map_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- CS_Map_probs_BT[,c(3)]
temp <- as.matrix(temp)
CS_Map_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
CS_Map_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
CS_Map_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(CS_Map.output)



######################
#CS:GO Gun Rounds BT##
######################



#отбираем нужные данные по матчам
CS_Gun_data <- CS[grep("round", CS$bet_type),]
CS_Gun_data_temp <- CS_Gun_data[CS_Gun_data$win == "draw", c(3,4,5,8,9)]
CS_future <- merge (CS_future,CS_Gun_data_temp,all.x = TRUE)
colnames(CS_future)[6] <- "gun_coeff1"
colnames(CS_future)[7] <- "gun_coeff2"
CS_Gun_data <- CS_Gun_data[CS_Gun_data$win != "draw" ,]

CS_Gun_data <- CS_Gun_data[,c(1,2,3,4,5,6,7,8,9)]
CS_Gun_data <- CS_Gun_data[(CS_Gun_data$date>Sys.time()-time_frame),]

#прописываем явно победителя и проигравшего
CS_Gun_data[CS_Gun_data$win == "win1", "winner"] <- CS_Gun_data[CS_Gun_data$win == "win1", "team1"]
CS_Gun_data[CS_Gun_data$win == "win1", "loser"] <- CS_Gun_data[CS_Gun_data$win == "win1", "team2"]
CS_Gun_data[CS_Gun_data$win == "win2", "winner"] <- CS_Gun_data[CS_Gun_data$win == "win2", "team2"]
CS_Gun_data[CS_Gun_data$win == "win2", "loser"] <- CS_Gun_data[CS_Gun_data$win == "win2", "team1"]

#задаем уровни
levels(CS_Gun_data[,"winner"]) <- unique(c(CS_Gun_data[,"winner"], CS_Gun_data[,"loser"]))
levels(CS_Gun_data[,"loser"]) <- unique(c(CS_Gun_data[,"winner"], CS_Gun_data[,"loser"]))

#результат - везде побела winner'а
CS_Gun_data$result <- rep (1, nrow (CS_Gun_data))

#вычищаем тех, кто не проигрывал или не побеждал, итеративно несколько раз
CS_Gun_data <- CS_Gun_data[CS_Gun_data$loser %in% CS_Gun_data$winner, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$winner %in% CS_Gun_data$loser, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$loser %in% CS_Gun_data$winner, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$winner %in% CS_Gun_data$loser, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$loser %in% CS_Gun_data$winner, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$winner %in% CS_Gun_data$loser, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$loser %in% CS_Gun_data$winner, ]
CS_Gun_data <- CS_Gun_data[CS_Gun_data$winner %in% CS_Gun_data$loser, ]

#делаем фреймы для модели
winner.frame <- as.data.frame(cbind(CS_Gun_data$winner, CS_Gun_data$bet))
loser.frame <- as.data.frame(cbind(CS_Gun_data$loser, CS_Gun_data$bet))
colnames(winner.frame) <- c("id", "bet")
colnames(loser.frame) <- c("id", "bet")
winner.frame$bet <- as.factor(winner.frame$bet)
loser.frame$bet <- as.factor(loser.frame$bet)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)

summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame,data = CS_Gun_data, 
                               id = "id"))

rm(winner.frame)
rm(loser.frame)

CS_Gun.output <- data.frame(BTabilities(dotamatch.model))
CS_Gun.output <- CS_Gun.output[!is.na (CS_Gun.output$ability),]
CS_Gun.output$Pr <- apply (CS_Gun.output, 1, function(x) (x[1] /x[2]))
CS_Gun.output$Pr <- abs(CS_Gun.output$Pr)
CS_Gun.output <- CS_Gun.output[(CS_Gun.output$Pr>Pr & !is.na(CS_Gun.output$s.e.)),]
CS_Gun.output <- CS_Gun.output[(CS_Gun.output$Pr>Pr & !is.na(CS_Gun.output$s.e.)),]
team_names <- rownames(CS_Gun.output)
CS_Gun.abilities <- CS_Gun.output$ability
names(CS_Gun.abilities) <- team_names

CS_Gun_probs_BT <- outer(CS_Gun.abilities, CS_Gun.abilities, prob_BT)
diag(CS_Gun_probs_BT) <- 0
CS_Gun_probs_BT <- melt(CS_Gun_probs_BT)
colnames(CS_Gun_probs_BT)[1] <- "team1"
colnames(CS_Gun_probs_BT)[2] <- "team2"
colnames(CS_Gun_probs_BT)[3] <- "Pr1"

#вероятность выигрыша 2-ой команды и коэффициенты для игры "в ноль"
temp <- CS_Gun_probs_BT[,c(3)]
temp <- as.matrix(temp)
CS_Gun_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
CS_Gun_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
CS_Gun_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
rm(temp)
rm(CS_Gun.output)



########
#Result#
########

if (length(CS_Map_probs_BT) >0){
  CS_future <- merge (CS_future, CS_Map_probs_BT,all.x=TRUE)
  CS_future$Pr1 <- NULL
  CS_future$Pr2 <- NULL
  colnames(CS_future)[6] <- "map_pred1"
  colnames(CS_future)[7] <- "map_pred2"
} else {
  CS_future$map_pred1 <- NA
  CS_future$map_pred2 <- NA
}

if (length(CS_Gun_probs_BT) >0){
  CS_future <- merge (CS_future, CS_Gun_probs_BT,all.x=TRUE)
  CS_future$Pr1 <- NULL
  CS_future$Pr2 <- NULL
  colnames(CS_future)[10] <- "gun_pred1"
  colnames(CS_future)[11] <- "gun_pred2"
} else {
  CS_future$gun_pred1 <- NA
  CS_future$gun_pred2 <- NA
}


CS_future[is.na(CS_future$map_coeff1),"map_coeff1"] <- 0
CS_future[is.na(CS_future$map_coeff2),"map_coeff2"] <- 0
CS_future[is.na(CS_future$map_pred1),"map_pred1"] <- 100
CS_future[is.na(CS_future$map_pred2),"map_pred2"] <- 100
CS_future[is.na(CS_future$gun_coeff1),"gun_coeff1"] <- 0
CS_future[is.na(CS_future$gun_coeff2),"gun_coeff2"] <- 0
CS_future[is.na(CS_future$gun_pred1),"gun_pred1"] <- 100
CS_future[is.na(CS_future$gun_pred2),"gun_pred2"] <- 100

CS_Result <- CS_future
CS_Result$map <- NA
CS_Result$gun <- NA
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "team1"]
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "team2"]
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "team1"]
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "team2"]

CS_Result$game <- "CS"
CS_Result <- CS_Result[,c(14,3,1,2,12,13)]
CS_Result <- CS_Result[(!is.na(CS_Result$gun) | !is.na(CS_Result$map)),]



Result <- merge(Result, CS_Result, all=TRUE)

#выгрузка в DropBox
setwd("C:/Users/Привет/Dropbox")
write.csv(Result, "Result.csv")
write.csv(D2_FB_WL, "D2_FB_WL.csv")
write.csv(LoL_FB_WL, "LoL_FB_WL.csv")

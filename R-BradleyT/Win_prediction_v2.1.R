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

prob_D2_FB <- function(D2_FB_1, D2_FB_2) {
  (D2_FB_1 + 1 - D2_FB_2)/2
}

Pr = 1.4 #граница отсечения отношения матожидания к стандартной ошибке, выше которой считаем результат верным
time_frame = 120 #временнАя граница анализа данных
bet_edge = 0.15 #граница разницы предсказания и коэффициента, выше которой ставим

#собираем данные из всех .csv за все дни
files = list.files(path = "D:/calculations/coeffgrabber/Data", pattern="*.csv")
setwd("D:/calculations/coeffgrabber/Data")
EGB <- read.csv("EGB.csv", header = FALSE, stringsAsFactors=FALSE)

for (i in 1:(length(files)-2)) 
{
  EGB_temp <- read.csv(files[i], header = FALSE, stringsAsFactors=FALSE)
  EGB <- merge(EGB, EGB_temp, all=TRUE)
}
EGB <- EGB[EGB$V11 != "draw",]
EGB_temp <- read.csv(files[length(files)-1], header = FALSE, stringsAsFactors=FALSE)
EGB <- merge(EGB, EGB_temp, all=TRUE)
rm(EGB_temp)

#немного чистим
EGB <- EGB[,c(1,10,2,5,6,11,8,3,4,7,9)]
colnames(EGB) <- c("bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "map")
EGB$date <- as.POSIXct(EGB$date)
EGB <- EGB[(EGB$date>"2016/03/01"),]

#еще чистим
EGB[EGB$team1 == "!Rebels!","team1"] <- "Rebels"
EGB[EGB$team2 == "!Rebels!","team2"] <- "Rebels"
EGB[EGB$team1 == "BrooDMotherS","team1"] <- "BroodMothers"
EGB[EGB$team2 == "BrooDMotherS","team2"] <- "BroodMothers"
EGB[EGB$team1 == "The Mongolz","team1"] <- "TheMongolz"
EGB[EGB$team2 == "The Mongolz","team2"] <- "TheMongolz"



######################
#D2_Map Bradley-Terry#
######################



#отбираем нужные данные по матчам
D2_Map_data <- EGB[EGB$bet_type == "GameResult",]
D2_Map_data_temp <- EGB[EGB$bet_type == "Total",]
D2_Map_data_temp <- D2_Map_data_temp[!(D2_Map_data_temp$series %in% D2_Map_data$series), ]
D2_Map_data <- merge(D2_Map_data, D2_Map_data_temp, all=TRUE)
D2_future <- D2_Map_data[D2_Map_data$date > Sys.time(), c(4,5,8,9)]
colnames(D2_future)[3] <- "map_coeff1"
colnames(D2_future)[4] <- "map_coeff2"
D2_Map_data <- D2_Map_data[D2_Map_data$win != "draw" ,]
rm(D2_Map_data_temp)

D2_Map_data <- D2_Map_data[,c(1,2,3,4,5,6,7,8,9)]
D2_Map_data <- D2_Map_data[(D2_Map_data$date>Sys.Date()-time_frame),]

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
D2_FB_data <- EGB[grep("lood", EGB$bet_type),]
D2_FB_data_temp <- EGB[EGB$bet_type == "FB",]
D2_FB_data <- merge(D2_FB_data, D2_FB_data_temp, all=TRUE)
rm(D2_FB_data_temp)
D2_FB_data_temp <- D2_FB_data[D2_FB_data$date > Sys.time(), c(4,5,8,9)]
D2_future <- merge (D2_future,D2_FB_data_temp,all.x = TRUE)
colnames(D2_future)[5] <- "fb_coeff1"
colnames(D2_future)[6] <- "fb_coeff2"
D2_FB_data <- D2_FB_data[D2_FB_data$win != "draw" ,]
rm(D2_FB_data_temp)

D2_FB_data <- D2_FB_data[,c(1,2,3,4,5,6,7,8,9)]
D2_FB_data <- D2_FB_data[(D2_FB_data$date>Sys.Date()-time_frame),]

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
D2_FB_probs <- outer(D2_FB.shares, D2_FB.shares, prob_D2_FB)
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
D2_Kills_data <- EGB[EGB$bet_type == "Kills10",]
D2_Kills_data_temp <- D2_Kills_data[D2_Kills_data$date > Sys.time(), c(4,5,8,9)]
D2_future <- merge (D2_future,D2_Kills_data_temp,all.x = TRUE)
colnames(D2_future)[7] <- "kills_coeff1"
colnames(D2_future)[8] <- "kills_coeff2"
rm(D2_Kills_data_temp)
D2_Kills_data <- D2_Kills_data[D2_Kills_data$win != "draw" ,]

D2_Kills_data <- D2_Kills_data[,c(1,2,3,4,5,6,7,8,9)]
D2_Kills_data <- D2_Kills_data[(D2_Kills_data$date>Sys.Date()-time_frame),]

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
colnames(D2_future)[9] <- "map_pred1"
colnames(D2_future)[10] <- "map_pred2"
} else {
  D2_future$map_pred1 <- NA
  D2_future$map_pred2 <- NA
}

if (length(D2_FB_probs_BT) >0){
D2_future <- merge (D2_future, D2_FB_probs_BT,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[11] <- "fb_pred1"
colnames(D2_future)[12] <- "fb_pred2"
} else {
  D2_future$fb_pred1 <- NA
  D2_future$fb_pred2 <- NA
}


D2_future <- merge (D2_future, D2_FB_probs,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[13] <- "fb_wl_pred1"
colnames(D2_future)[14] <- "fb_wl_pred2"

if (length(D2_Kills_probs_BT) >0){
D2_future <- merge (D2_future, D2_Kills_probs_BT,all.x=TRUE)
D2_future$Pr1 <- NULL
D2_future$Pr2 <- NULL
colnames(D2_future)[15] <- "kills_pred1"
colnames(D2_future)[16] <- "kills_pred2"
} else {
  D2_future$kills_pred1 <- NA
  D2_future$kills_pred2 <- NA
}

D2_future[is.na(D2_future$map_pred1),"map_pred1"] <- 100
D2_future[is.na(D2_future$map_pred2),"map_pred2"] <- 100
D2_future[is.na(D2_future$fb_pred1),"fb_pred1"] <- 100
D2_future[is.na(D2_future$fb_pred2),"fb_pred2"] <- 100
D2_future[is.na(D2_future$fb_wl_pred1),"fb_wl_pred1"] <- 100
D2_future[is.na(D2_future$fb_wl_pred2),"fb_wl_pred2"] <- 100
D2_future[is.na(D2_future$kills_pred1),"kills_pred1"] <- 100
D2_future[is.na(D2_future$kills_pred2),"kills_pred2"] <- 100

Result <- D2_future
Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "map"] <- Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "team1"]
Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "map"] <- Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "team2"]
Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "fb"] <- Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "team1"]
Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "fb"] <- "team2"
Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "fb"] <- Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "team1"]
Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "fb"] <- Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "team2"]
Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "kills"] <- Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "team1"]
Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "kills"] <- Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "team2"]

Result$game <- "Dota"
Result <- Result[,c(20,1,2,17,18,19)]
Result <- Result[(!is.na(Result$map) | !is.na(Result$fb) | !is.na(Result$kills)),]


#выгрузка в DropBox
setwd("C:/Users/Привет/Dropbox")
write.csv(Result, "Result.csv")

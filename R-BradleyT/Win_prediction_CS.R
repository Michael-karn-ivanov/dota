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

Pr = 1.4 #граница отсечени€ отношени€ матожидани€ к стандартной ошибке, выше которой считаем результат верным
time_frame = 60*24*60*60 #временнј€ граница анализа данных в секундах
bet_edge = 0.1 #граница разницы предсказани€ и коэффициента, выше которой ставим

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
colnames(EGB) <- c("game", "bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "gun")
EGB$date <- as.POSIXct(EGB$date)
EGB <- EGB[(EGB$date>"2016/03/01"),]



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

#прописываем €вно победител€ и проигравшего
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

#делаем фреймы дл€ модели
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

#веро€тность выигрыша 2-ой команды и коэффициенты дл€ игры "в ноль"
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

#прописываем €вно победител€ и проигравшего
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

#делаем фреймы дл€ модели
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

#веро€тность выигрыша 2-ой команды и коэффициенты дл€ игры "в ноль"
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


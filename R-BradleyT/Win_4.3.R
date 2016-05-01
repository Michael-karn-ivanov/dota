library(readr)
library(BradleyTerry2)
library(reshape)
source("helpers/LoadData.R")
source("helpers/bet43.R")

RootPath <- "C:/dev/dota/"
OutputPath <- "C:/dev/dota/results"
Pr = 1.4 #??????? ????????? ????????? ??????????? ? ??????????? ??????, ???? ??????? ??????? ????????? ??????
time_frame = 60*24*60*60 #????????? ??????? ??????? ?????? ? ????????
bet_edge = 0 #??????? ??????? ???????????? ? ????????????, ???? ??????? ??????
KellyBase = 350

EGB <- LoadDataSet(RootPath)

predict <- function(dataset, game, type) {
  gameset <- dataset[(dataset$game == game), -which(names(dataset) %in% c("game"))]
  if(type == 'total') {
    data <- gameset[gameset$bet_type == "GameResult",]
    data_temp <- gameset[gameset$bet_type == "Total",]
    data_temp <- data_temp[!(data_temp$series %in% data$series), ]
    data <- merge(data, data_temp, all=TRUE)
  }
  if(type == "fb") {
    data <- gameset[grep("lood", gameset$bet_type),]
    data_temp <- gameset[gameset$bet_type == "FB",]
    data <- merge(data, data_temp, all=TRUE)
  }
  if(type == "kills10") {
    data <- gameset[gameset$bet_type == "Kills10",]
  }
  
  future <- data[data$win == "draw", c(3,4,5,8,9)]
  probs_BT <- build_model(data, type)
  future <- map_prediction(future, probs_BT, "pred1", "pred2")
  if (type == "fb") {
    probs <- predicted_FB_probs
    future_probs <- future
    future_probs$winner <- NA
    future_probs$Kelly <- NA
    future_probs <- map_prediction(future_probs, probs, "pred1", "pred2")
    future_probs[future_probs$coeff1 - future_probs$pred1 > 0, "winner"] <- future[future_probs$coeff1 - future_probs$pred1 > 0, "team1"]
    future_probs[future_probs$coeff1 - future_probs$pred1 > 0, "Kelly"] <- sapply(rownames(future_probs[future_probs$coeff1 - future_probs$pred1 > 0,]), function(x) kelly(future_probs, x, "coeff1", "pred1"))
    future_probs[future_probs$coeff2 - future_probs$pred2 > 0, "winner"] <- future[future_probs$coeff2 - future_probs$pred2 > 0, "team2"]
    future_probs[future_probs$coeff2 - future_probs$pred2 > 0, "Kelly"] <- sapply(rownames(future_probs[future_probs$coeff2 - future_probs$pred2 > 0,]), function(x) kelly(future_probs, x, "coeff2", "pred2"))
    probs_aggr <- future_probs[!is.na(future_probs$winner), c("team1", "team2", "date", "Kelly")]
    probs_aggr$bet <- NA
    probs_aggr$bet <- sapply(rownames(future_probs[!is.na(future_probs$winner),]), function(x) paste(type, future_probs[x, "winner"], sep = ":"))
    probs_aggr$game <- game
    probs_aggr<- probs_aggr[c("game", "team1", "team2", "date", "bet", "Amount", "Kelly")]
  } else {
    probs_aggr <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(probs_aggr) <- c("game", "team1", "team2", "date", "bet", "Amount", "Kelly")
  }
  future$winner <- NA
  future$Kelly <- NA
  future[future$coeff1 - future$pred1 > 0, "winner"] <- future[future$coeff1 - future$pred1 > 0, "team1"]
  future[future$coeff1 - future$pred1 > 0, "Kelly"] <- sapply(rownames(future[future$coeff1 - future$pred1 > 0,]), function(x) kelly(future, x, "coeff1", "pred1"))
  future[future$coeff2 - future$pred2 > 0, "winner"] <- future[future$coeff2 - future$pred2 > 0, "team2"]
  future[future$coeff2 - future$pred2 > 0, "Kelly"] <- sapply(rownames(future[future$coeff2 - future$pred2 > 0,]), function(x) kelly(future, x, "coeff2", "pred2"))
  Aggr <- future[!is.na(future$winner), c("team1", "team2", "date", "Kelly")]
  Aggr$bet <- sapply(rownames(future[!is.na(future$winner),]), function(x) paste(type, future[x, "winner"], sep = ":"))
  Aggr$game <- game
  Aggr<- Aggr[c("game", "team1", "team2", "date", "bet", "Amount", "Kelly")]
  Aggr<-rbind(Aggr, probs_aggr)
  return(Aggr)
}

#D2_total <- predict(EGB, 'Dota2', 'total')
D2_fb <- predict(EGB, 'Dota2', 'fb')


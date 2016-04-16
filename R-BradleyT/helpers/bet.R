library(BradleyTerry2)

prob_FB <- function(D2_FB_1, D2_FB_2) {
  (D2_FB_1 + 1 - D2_FB_2)/2
}

prob_BT <- function(ability_1, ability_2) {
  inv_logit(ability_1 - ability_2)
}

inv_logit <- function(p) {
  exp(p) / (1 + exp(p))
}


build_model <- function(dataset, strategy) {
  time_frame = 60*24*60*60
  predicted <- dataset[dataset$win != "draw" ,]
  predicted <- predicted[,c("bet", "series", "date", "team1", "team2", "win", "league", "coeff1", "coeff2")]
  predicted <- predicted[(predicted$date>Sys.time()-time_frame),]
  
  predicted[predicted$win == "win1", "winner"] <- predicted[predicted$win == "win1", "team1"]
  predicted[predicted$win == "win1", "loser"] <- predicted[predicted$win == "win1", "team2"]
  predicted[predicted$win == "win2", "winner"] <- predicted[predicted$win == "win2", "team2"]
  predicted[predicted$win == "win2", "loser"] <- predicted[predicted$win == "win2", "team1"]
  
  levels(predicted[,"winner"]) <- unique(c(predicted[,"winner"], predicted[,"loser"]))
  levels(predicted[,"loser"]) <- unique(c(predicted[,"winner"], predicted[,"loser"]))
  
  predicted$result <- rep (1, nrow (predicted))
  
  if("dota_fb" == strategy || "lol_fb" == strategy) {
    predicted_pure_lose <- predicted[, c(1,11)]
    predicted_pure_lose$num <- apply (predicted_pure_lose, 1, function(x) (sum(predicted_pure_lose$loser == x[2])))
    predicted_pure_lose$bet <- NULL
    predicted_pure_lose <- unique(predicted_pure_lose)
    colnames(predicted_pure_lose) <- c("team", "loses")
    predicted_pure_win <- predicted[, c(1,10)]
    predicted_pure_win$num <- apply (predicted_pure_win, 1, function(x) (sum(predicted_pure_win$winner == x[2])))
    predicted_pure_win$bet <- NULL
    predicted_pure_win <- unique(predicted_pure_win)
    colnames(predicted_pure_win) <- c("team", "wins")
    predicted_FB_WL <- merge (predicted_pure_win, predicted_pure_lose, all = TRUE)
    rm (predicted_pure_win)
    rm (predicted_pure_lose)
    
    predicted_FB_WL[is.na(predicted_FB_WL$wins),"wins"] <- 0
    predicted_FB_WL[is.na(predicted_FB_WL$loses),"loses"] <- 0
    temp <- predicted_FB_WL[,c(2,3)]
    temp <- as.matrix(temp)
    predicted_FB_WL$share <- apply (temp, 1, function(x) (x[1]/(x[1]+x[2])))
    predicted_FB_WL$total <- apply (temp, 1, function(x) (x[1]+x[2]))
    rm(temp)
    predicted_FB_WL[predicted_FB_WL$total < 5,"share"] <- NA
    
    #??????? ??????????? ???????? ?????? ???????
    predicted_FB.shares <- predicted_FB_WL$share
    names(predicted_FB.shares) <- predicted_FB_WL$team
    predicted_FB_probs <- outer(predicted_FB.shares, predicted_FB.shares, prob_FB)
    diag(predicted_FB_probs) <- 0
    predicted_FB_probs <- melt(predicted_FB_probs)
    
    colnames(predicted_FB_probs)[1] <- "team1"
    colnames(predicted_FB_probs)[2] <- "team2"
    colnames(predicted_FB_probs)[3] <- "Pr1"
    
    #??????????? ???????? 2-?? ??????? ? ???????????? ??? ???? "? ????"
    temp <- predicted_FB_probs[,c(3)]
    temp <- as.matrix(temp)
    predicted_FB_probs$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
    predicted_FB_probs$coeff1 <- apply (temp, 1, function(x) 1/x[1])
    predicted_FB_probs$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
    rm(temp)
    predicted_FB_WL <<- predicted_FB_WL
    predicted_FB_probs <<- predicted_FB_probs
  }
  
  predicted <- predicted[predicted$loser %in% predicted$winner, ]
  predicted <- predicted[predicted$winner %in% predicted$loser, ]
  predicted <- predicted[predicted$loser %in% predicted$winner, ]
  predicted <- predicted[predicted$winner %in% predicted$loser, ]
  predicted <- predicted[predicted$loser %in% predicted$winner, ]
  predicted <- predicted[predicted$winner %in% predicted$loser, ]
  predicted <- predicted[predicted$loser %in% predicted$winner, ]
  predicted <- predicted[predicted$winner %in% predicted$loser, ]
  
  wframe <<- as.data.frame(cbind(predicted$winner, predicted$bet))
  lframe <<- as.data.frame(cbind(predicted$loser, predicted$bet))
  colnames(wframe) <<- c("id", "bet")
  colnames(lframe) <<- c("id", "bet")
  wframe$bet <<- as.factor(wframe$bet)
  lframe$bet <<- as.factor(lframe$bet)
  wframe$id <<- as.factor(wframe$id)
  lframe$id <<- as.factor(lframe$id)
  
  model <- BTm(result, player1 = wframe, 
                                 player2 = lframe,data = predicted, 
                                 id = "id")
  
  output <- data.frame(BTabilities(model))
  output <- output[!is.na (output$ability),]
  output$Pr <- apply (output, 1, function(x) (x[1] /x[2]))
  output$Pr <- abs(output$Pr)
  output <- output[(output$Pr>Pr & !is.na(output$s.e.)),]
  output <- output[(output$Pr>Pr & !is.na(output$s.e.)),]
  team_names <- rownames(output)
  output.abilities <- output$ability
  names(output.abilities) <- team_names
  
  output_probs_BT <- outer(output.abilities, output.abilities, prob_BT)
  diag(output_probs_BT) <- 0
  if(length(output_probs_BT) > 0) {
    output_probs_BT <- melt(output_probs_BT)
  
    colnames(output_probs_BT)[1] <- "team1"
    colnames(output_probs_BT)[2] <- "team2"
    colnames(output_probs_BT)[3] <- "Pr1"
    
    temp <- output_probs_BT[,c(3)]
    temp <- as.matrix(temp)
    output_probs_BT$Pr2 <- apply (temp, 1, function(x) (1-x[1]))
    output_probs_BT$coeff1 <- apply (temp, 1, function(x) 1/x[1])
    output_probs_BT$coeff2 <- apply (temp, 1, function(x) (1/(1-x[1])))
  }
  return(output_probs_BT)
}

map_prediction <- function(future, model, coeff1Name, coeff2Name) {
  result <- future
  if (length(model) >0){
    result <- merge (result, model,all.x=TRUE)
    result$Pr1 <- NULL
    result$Pr2 <- NULL
    colnames(result)[length(colnames(result)) - 1] <- coeff1Name
    colnames(result)[length(colnames(result))] <- coeff2Name
  } else {
    result[[coeff1Name]] <- NA
    result[[coeff2Name]] <- NA
  }
  result[is.na(result[[coeff1Name]]), coeff1Name] <- 100
  result[is.na(result[[coeff2Name]]), coeff2Name] <- 100
  return(result)
}

kelly <- function(dataset, rowname, coeff_name, prediction_name) {
  K <- dataset[rowname, coeff_name]
  V <- dataset[rowname, prediction_name]
  up <- K * V - 1
  bottom <- K - 1
  return(up/bottom)
}
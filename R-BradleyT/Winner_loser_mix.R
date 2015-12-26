library(readr)
library(BradleyTerry2)
library(reshape)
#source("Kelly.R")

#inv_logit <- function(p) {
#  exp(p) / (1 + exp(p))
#}

inv_logit <- function(p) {
  exp(p)
}

prob_BT <- function(ability_1, ability_2) {
  inv_logit(ability_1 - ability_2)
}

matches <- read.csv("matches.csv", stringsAsFactors=FALSE)

matches$winner_id <- NA
matches[matches$winner == "dire", "winner_id"] <- matches[matches$winner == "dire", "dire_id"]
matches[matches$winner == "radiant", "winner_id"] <- matches[matches$winner == "radiant", "radiant_id"]
matches$loser_id <- NA
matches[matches$winner == "dire", "loser_id"] <- matches[matches$winner == "dire", "radiant_id"]
matches[matches$winner == "radiant", "loser_id"] <- matches[matches$winner == "radiant", "dire_id"]

levels(matches[,"winner_id"]) <- unique(c(matches[,"winner_id"], matches[,"loser_id"]))
levels(matches[,"loser_id"]) <- unique(c(matches[,"winner_id"], matches[,"loser_id"]))

matches$result <- rep (1, nrow (matches))

matches <- matches[!is.na (matches$winner_id),]
matches <- matches[!is.na (matches$loser_id),]

matches <- matches[matches$loser_id %in% matches$winner_id, ]
matches <- matches[matches$winner_id %in% matches$loser_id, ]


winner.frame <- as.data.frame(cbind(matches$winner_id, matches$match_id))
loser.frame <- as.data.frame(cbind(matches$loser_id, matches$match_id))
colnames(winner.frame) <- c("id", "match_id")
colnames(loser.frame) <- c("id", "match_id")
winner.frame$match_id <- as.factor(winner.frame$match_id)
loser.frame$match_id <- as.factor(loser.frame$match_id)
winner.frame$id <- as.factor(winner.frame$id)
loser.frame$id <- as.factor(loser.frame$id)


summary(dotamatch.model <- BTm(result, player1 = winner.frame, 
                               player2 = loser.frame, data = matches, 
                               id = "id"))

dotamatch.output <- data.frame(BTabilities(dotamatch.model))
dotamatch.output <- dotamatch.output[(dotamatch.output$ability/dotamatch.output$s.e.>1.4 & !is.na(dotamatch.output$s.e.)),]
team_names <- rownames(dotamatch.output)
dotamatch.abilities <- dotamatch.output$ability
names(dotamatch.abilities) <- team_names

dota_probs <- outer(dotamatch.abilities, dotamatch.abilities, prob_BT)
diag(dota_probs) <- 0
dota_probs <- melt(dota_probs)
colnames(dota_probs)[1] <- "Team1"
colnames(dota_probs)[2] <- "Team2"

coeffs <- read.csv("Coefs.csv", stringsAsFactors=FALSE)
coeffs <- merge(coeffs, dota_probs)
coeffs_test <- coeffs[,c(5,6,11)]
coeffs_test <- as.matrix(coeffs_test)

coeffs$answ1 <- NA
coeffs$answ2 <- NA
coeffs$answ1 <- apply (coeffs_test, 1, function(x) (x[3] * x[1] - 1)/(x[1] - 1))
coeffs$answ2 <- apply (coeffs_test, 1, function(x) ((1-x[3]) * x[2] - 1)/(x[2] - 1))
coeffs <- coeffs[!(coeffs$answ1<0 & coeffs$answ2<0),]
coeffs <- coeffs[!(is.na(coeffs$Match_Id)),]
# coeffs$result1 <- NA
# coeffs$result2 <- NA




# dota_probs_test <- dota_probs[(dota_probs$Team1=="2642171"),]



# head (dota_probs, 90000)

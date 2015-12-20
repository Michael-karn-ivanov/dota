library(readr)
library(BradleyTerry2)

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
length(unique(matches$winner_id))
length(unique(matches$loser_id))

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




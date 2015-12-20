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

nrow (matches)
matches$result <- rep (1, nrow (matches))

matches <- matches[!is.na (matches$winner_id),]
matches <- matches[!is.na (matches$loser_id),]

length(unique(matches$winner_id))
length(unique(matches$loser_id))

test <- matches[matches$loser_id %in% matches$winner_id, ]
test <- test[test$winner_id %in% matches$loser_id, ]
length(unique(test$winner_id))
length(unique(test$loser_id))

summary(dotamatch.model <- BTm(result, player1 = test$winner_id, 
                               player2 = test$loser_id, data = test, id = "match_id"))


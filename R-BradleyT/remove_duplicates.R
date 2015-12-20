library(readr)

matches <- read.table("../data/matches.csv", sep="\t", header=FALSE)
matches <- unique(matches)
colnames(matches) <- c("match_id", "date", "radiant_id", "dire_id", "winner", "firstblood", "league_id")
write_csv(matches, "matches.csv")

teams <- read.table("../data/teams.csv", sep=",", header=FALSE)
teams <- unique(teams)
colnames(teams) <- c("team_id", "name", "href")
write_csv(teams, "teams.csv")

leagues <- read.table("../data/leagues.csv", sep="\t", header=FALSE)
leagues <- unique(leagues)
colnames(leagues) <- c("league_id", "name", "type", "prize")
write_csv(leagues, "leagues.csv")

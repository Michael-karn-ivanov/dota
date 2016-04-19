library(readr)
library(BradleyTerry2)
library(reshape)
source("helpers/LoadData.R")
source("helpers/bet.R")

RootPath <- "C:/dev/dota/"
OutputPath <- "C:/dev/dota/results"
Pr = 1.4 #??????? ????????? ????????? ??????????? ? ??????????? ??????, ???? ??????? ??????? ????????? ??????
time_frame = 60*24*60*60 #????????? ??????? ??????? ?????? ? ????????
bet_edge = 0 #??????? ??????? ???????????? ? ????????????, ???? ??????? ??????
KellyBase = 200

EGB <- LoadDataSet(RootPath)

##########################################################
#D2
##########################################################
D2 <- EGB[(EGB$game == 'Dota2'), -which(names(EGB) %in% c("game"))]

##########################
#dota_map
##########################
D2_Map_data <- D2[D2$bet_type == "GameResult",]
D2_Map_data_temp <- D2[D2$bet_type == "Total",]
D2_Map_data_temp <- D2_Map_data_temp[!(D2_Map_data_temp$series %in% D2_Map_data$series), ]
D2_Map_data <- merge(D2_Map_data, D2_Map_data_temp, all=TRUE)
D2_future <- D2_Map_data[D2_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(D2_future)[4] <- "map_coeff1"
colnames(D2_future)[5] <- "map_coeff2"

D2_Map_probs_BT <- build_model(D2_Map_data, "dota_map")
rm(lframe)
rm(wframe)
##########################
#dota_fb
##########################
D2_FB_data <- D2[grep("lood", D2$bet_type),]
D2_FB_data_temp <- D2[D2$bet_type == "FB",]
D2_FB_data <- merge(D2_FB_data, D2_FB_data_temp, all=TRUE)
rm(D2_FB_data_temp)
D2_FB_data_temp <- D2_FB_data[D2_FB_data$win == "draw", c(3,4,5,8,9)]
D2_future <- merge (D2_future,D2_FB_data_temp,all.x = TRUE)
colnames(D2_future)[6] <- "fb_coeff1"
colnames(D2_future)[7] <- "fb_coeff2"

D2_FB_probs_BT <- build_model(D2_FB_data, "dota_fb")
D2_FB_probs <- predicted_FB_probs
D2_FB_WL <- predicted_FB_WL
rm(lframe)
rm(wframe)
rm(predicted_FB_probs)
rm(predicted_FB_WL)
#####################
#dota_kills
#####################
D2_Kills_data <- D2[D2$bet_type == "Kills10",]
D2_Kills_data_temp <- D2_Kills_data[D2_Kills_data$win == "draw", c(3,4,5,8,9)]
D2_future <- merge (D2_future,D2_Kills_data_temp,all.x = TRUE)
colnames(D2_future)[8] <- "kills_coeff1"
colnames(D2_future)[9] <- "kills_coeff2"
rm(D2_Kills_data_temp)

D2_Kills_probs_BT <- build_model(D2_Kills_data, "dota_kills")
rm(lframe)
rm(wframe)
########
#Result#
########

D2_future <- map_prediction(D2_future, D2_Map_probs_BT, "map_pred1", "map_pred2")
D2_future <- map_prediction(D2_future, D2_FB_probs_BT, "fb_pred1", "fb_pred2")
D2_future <- map_prediction(D2_future, D2_FB_probs, "fb_wl_pred1", "fb_wl_pred2")
D2_future <- map_prediction(D2_future, D2_Kills_probs_BT, "kills_pred1", "kills_pred2")


D2_future[is.na(D2_future$map_coeff1),"map_coeff1"] <- 0
D2_future[is.na(D2_future$map_coeff2),"map_coeff2"] <- 0
D2_future[is.na(D2_future$fb_coeff1),"fb_coeff1"] <- 0
D2_future[is.na(D2_future$fb_coeff2),"fb_coeff2"] <- 0
D2_future[is.na(D2_future$kills_coeff1),"kills_coeff1"] <- 0
D2_future[is.na(D2_future$kills_coeff2),"kills_coeff2"] <- 0

D2_Result <- D2_future
D2_Result$map <- NA
D2_Result$fb <- NA
D2_Result$kills <- NA
D2_Result$KellyMap <- NA
D2_Result$KellyFB <- NA
D2_Result$KellyKills <- NA
D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "team1"]
D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "KellyMap"] <- sapply(rownames(D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge,]), function(x) kelly(D2_Result, x, "map_coeff1", "map_pred1"))
D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "team2"]
D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "KellyMap"] <- sapply(rownames(D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge,]), function(x) kelly(D2_Result, x, "map_coeff2", "map_pred2"))
D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "KellyFB"] <- sapply(rownames(D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge,]), function(x) kelly(D2_Result, x, "fb_coeff1", "fb_wl_pred1"))
D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "KellyFB"]<- sapply(rownames(D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge,]), function(x) kelly(D2_Result, x, "fb_coeff2", "fb_wl_pred2"))
D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "KellyFB"] <- sapply(rownames(D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge,]), function(x) kelly(D2_Result, x, "fb_coeff1", "fb_pred1"))
D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "KellyFB"] <- sapply(rownames(D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge,]), function(x) kelly(D2_Result, x, "fb_coeff2", "fb_pred2"))
D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "team1"]
D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "KellyKills"] <- sapply(rownames(D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge,]), function(x) kelly(D2_Result, x, "kills_coeff1", "kills_pred1"))
D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "team2"]
D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "KellyKills"] <- sapply(rownames(D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge,]), function(x) kelly(D2_Result, x, "kills_coeff2", "kills_pred2"))


D2_Aggr_Map <- D2_Result[!is.na(D2_Result$map), c("team1", "team2", "date", "KellyMap")]
D2_Aggr_Map$bet <- sapply(rownames(D2_Result[!is.na(D2_Result$map),]), function(x) paste("Map:", D2_Result[x, "map"]))
colnames(D2_Aggr_Map) <- c("team1", "team2", "date", "Kelly", "bet")
D2_Aggr_FB <- D2_Result[!is.na(D2_Result$fb), c("team1", "team2", "date", "KellyFB")]
D2_Aggr_FB$bet <- sapply(rownames(D2_Result[!is.na(D2_Result$fb),]), function(x) paste("First blood:", D2_Result[x, "fb"]))
colnames(D2_Aggr_FB) <- c("team1", "team2", "date", "Kelly", "bet")
D2_Aggr_Kills <- D2_Result[!is.na(D2_Result$kills), c("team1", "team2", "date", "KellyKills")]
D2_Aggr_Kills$bet <- sapply(rownames(D2_Result[!is.na(D2_Result$kills),]), function(x) paste("Kills:", D2_Result[x, "kills"]))
colnames(D2_Aggr_Kills) <- c("team1", "team2", "date", "Kelly", "bet")

D2_Aggr <- rbind(D2_Aggr_Map, D2_Aggr_FB)
D2_Aggr <- rbind(D2_Aggr, D2_Aggr_Kills)
D2_Aggr$game <- "Dota"

D2_Result$game <- "Dota"
D2_Result <- D2_Result[,c("game", "date", "team1", "team2", "map", "fb", "kills")]
D2_Result <- D2_Result[(!is.na(D2_Result$map) | !is.na(D2_Result$fb) | !is.na(D2_Result$kills)),]



################################################################################################
##LoL###
################################################################################################

LoL <- EGB[(EGB$game == 'LoL'), -which(names(EGB) %in% c("game"))]

######################
#LoL_Map Bradley-Terry#
######################

LoL_Map_data <- LoL[LoL$bet_type == "GameResult",]
LoL_Map_data_temp <- LoL[LoL$bet_type == "Total",]
LoL_Map_data_temp <- LoL_Map_data_temp[!(LoL_Map_data_temp$series %in% LoL_Map_data$series), ]
LoL_Map_data <- merge(LoL_Map_data, LoL_Map_data_temp, all=TRUE)
LoL_future <- LoL_Map_data[LoL_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(LoL_future)[4] <- "map_coeff1"
colnames(LoL_future)[5] <- "map_coeff2"

LoL_Map_probs_BT <- build_model(LoL_Map_data, "lol_map")
rm(lframe)
rm(wframe)
##########################
#LoL_FB Bradley-Terry + WL#
##########################

LoL_FB_data <- LoL[grep("lood", LoL$bet_type),]
LoL_FB_data_temp <- LoL[LoL$bet_type == "FB",]
LoL_FB_data <- merge(LoL_FB_data, LoL_FB_data_temp, all=TRUE)
rm(LoL_FB_data_temp)
LoL_FB_data_temp <- LoL_FB_data[LoL_FB_data$win == "draw", c(3,4,5,8,9)]
LoL_future <- merge (LoL_future,LoL_FB_data_temp,all.x = TRUE)
colnames(LoL_future)[6] <- "fb_coeff1"
colnames(LoL_future)[7] <- "fb_coeff2"

LoL_FB_probs_BT <- build_model(LoL_FB_data, "lol_fb")
LoL_FB_probs <- predicted_FB_probs
LoL_FB_WL <- predicted_FB_WL
rm(lframe)
rm(wframe)
rm(predicted_FB_probs)
rm(predicted_FB_WL)
#####################
#Kills Bradley-Terry#
#####################

LoL_Kills_data <- LoL[LoL$bet_type == "Kills10",]
LoL_Kills_data_temp <- LoL_Kills_data[LoL_Kills_data$win == "draw", c(3,4,5,8,9)]
LoL_future <- merge (LoL_future,LoL_Kills_data_temp,all.x = TRUE)
colnames(LoL_future)[8] <- "kills_coeff1"
colnames(LoL_future)[9] <- "kills_coeff2"
rm(LoL_Kills_data_temp)

LoL_Kills_probs_BT <- build_model(LoL_Kills_data, "lol_kills")
rm(lframe)
rm(wframe)
########
#Result#
########

LoL_future <- map_prediction(LoL_future, LoL_Map_probs_BT, "map_pred1", "map_pred2")
LoL_future <- map_prediction(LoL_future, LoL_FB_probs_BT, "fb_pred1", "fb_pred2")
LoL_future <- map_prediction(LoL_future, LoL_FB_probs, "fb_wl_pred1", "fb_wl_pred2")
LoL_future <- map_prediction(LoL_future, LoL_Kills_probs_BT, "kills_pred1", "kills_pred2")


LoL_future[is.na(LoL_future$map_coeff1),"map_coeff1"] <- 0
LoL_future[is.na(LoL_future$map_coeff2),"map_coeff2"] <- 0
LoL_future[is.na(LoL_future$fb_coeff1),"fb_coeff1"] <- 0
LoL_future[is.na(LoL_future$fb_coeff2),"fb_coeff2"] <- 0
LoL_future[is.na(LoL_future$kills_coeff1),"kills_coeff1"] <- 0
LoL_future[is.na(LoL_future$kills_coeff2),"kills_coeff2"] <- 0

LoL_Result <- LoL_future
LoL_Result$map <- NA
LoL_Result$fb <- NA
LoL_Result$kills <- NA
LoL_Result$KellyMap <- NA
LoL_Result$KellyFB <- NA
LoL_Result$KellyKills <- NA
LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "KellyMap"] <- sapply(rownames(LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge,]), function(x) kelly(LoL_Result, x, "map_coeff1", "map_pred1"))
LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "KellyMap"] <- sapply(rownames(LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge,]), function(x) kelly(LoL_Result, x, "map_coeff2", "map_pred2"))
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "KellyFB"] <- sapply(rownames(LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge,]), function(x) kelly(LoL_Result, x, "fb_coeff1", "fb_wl_pred1"))
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "KellyFB"] <- sapply(rownames(LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge,]), function(x) kelly(LoL_Result, x, "fb_coeff2", "fb_wl_pred2"))
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "KellyFB"] <- sapply(rownames(LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge,]), function(x) kelly(LoL_Result, x, "fb_coeff1", "fb_pred1"))
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "KellyFB"] <- sapply(rownames(LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge,]), function(x) kelly(LoL_Result, x, "fb_coeff2", "fb_pred2"))
LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "KellyKills"] <- sapply(rownames(LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge,]), function(x) kelly(LoL_Result, x, "kills_coeff1", "kills_pred1"))
LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "KellyKills"] <- sapply(rownames(LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge,]), function(x) kelly(LoL_Result, x, "kills_coeff2", "kills_pred2"))

LoL_Aggr_Map <- LoL_Result[!is.na(LoL_Result$map), c("team1", "team2", "date", "KellyMap")]
LoL_Aggr_Map$bet <- sapply(rownames(LoL_Result[!is.na(LoL_Result$map),]), function(x) paste("Map:", LoL_Result[x, "map"]))
colnames(LoL_Aggr_Map) <- c("team1", "team2", "date", "Kelly", "bet")
LoL_Aggr_FB <- LoL_Result[!is.na(LoL_Result$fb), c("team1", "team2", "date", "KellyFB")]
LoL_Aggr_FB$bet <- sapply(rownames(LoL_Result[!is.na(LoL_Result$fb),]), function(x) paste("First blood:", LoL_Result[x, "fb"]))
colnames(LoL_Aggr_FB) <- c("team1", "team2", "date", "Kelly", "bet")
LoL_Aggr_Kills <- LoL_Result[!is.na(LoL_Result$kills), c("team1", "team2", "date", "KellyKills")]
LoL_Aggr_Kills$bet <- sapply(rownames(LoL_Result[!is.na(LoL_Result$kills),]), function(x) paste("Kills:", LoL_Result[x, "kills"]))
colnames(LoL_Aggr_Kills) <- c("team1", "team2", "date", "Kelly", "bet")

LoL_Aggr <- rbind(LoL_Aggr_Map, LoL_Aggr_FB)
LoL_Aggr <- rbind(LoL_Aggr, LoL_Aggr_Kills)
LoL_Aggr$game <- "LoL"


LoL_Result$game <- "LoL"
LoL_Result <- LoL_Result[,c("game", "date", "team1", "team2", "map", "fb", "kills")]
LoL_Result <- LoL_Result[(!is.na(LoL_Result$map) | !is.na(LoL_Result$fb) | !is.na(LoL_Result$kills)),]

Result <- merge(D2_Result, LoL_Result, all = TRUE)



########
#CS:GO##
########
CS <- EGB[(EGB$game == 'Counter-Strike'), -which(names(EGB) %in% c("game"))]

######################
#CS_Map Bradley-Terry#
######################
CS_Map_data <- CS[CS$bet_type == "GameResult",]
CS_Map_data_temp <- CS[CS$bet_type == "Total",]
CS_Map_data_temp <- CS_Map_data_temp[!(CS_Map_data_temp$series %in% CS_Map_data$series), ]
CS_Map_data <- merge(CS_Map_data, CS_Map_data_temp, all=TRUE)
CS_future <- CS_Map_data[CS_Map_data$win == "draw", c(3,4,5,8,9)]
colnames(CS_future)[4] <- "map_coeff1"
colnames(CS_future)[5] <- "map_coeff2"

CS_Map_probs_BT <- build_model(CS_Map_data, "cs_map")
rm(lframe)
rm(wframe)

######################
#CS:GO Gun Rounds BT##
######################
CS_Gun_data <- CS[grep("round", CS$bet_type),]
CS_Gun_data_temp <- CS_Gun_data[CS_Gun_data$win == "draw", c(3,4,5,8,9)]
CS_future <- merge (CS_future,CS_Gun_data_temp,all.x = TRUE)
colnames(CS_future)[6] <- "gun_coeff1"
colnames(CS_future)[7] <- "gun_coeff2"

CS_Gun_probs_BT <- build_model(CS_Gun_data, "cs_gun")
rm(lframe)
rm(wframe)
########
#Result#
########

CS_future <- map_prediction(CS_future, CS_Map_probs_BT, "map_pred1", "map_pred2")
CS_future <- map_prediction(CS_future, CS_Gun_probs_BT, "gun_pred1", "gun_pred2")

CS_future[is.na(CS_future$map_coeff1),"map_coeff1"] <- 0
CS_future[is.na(CS_future$map_coeff2),"map_coeff2"] <- 0
CS_future[is.na(CS_future$gun_coeff1),"gun_coeff1"] <- 0
CS_future[is.na(CS_future$gun_coeff2),"gun_coeff2"] <- 0

CS_Result <- CS_future
CS_Result$map <- NA
CS_Result$gun <- NA
CS_Result$KellyMap <- NA
CS_Result$KellyGun <- NA
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "team1"]
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "KellyMap"] <- sapply(rownames(CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge,]), function(x) kelly(CS_Result, x, "map_coeff1", "map_pred1"))
CS_Result[CS_future$map_coeff2 - CS_future$map_pred2 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff2 - CS_future$map_pred2 > bet_edge, "team2"]
CS_Result[CS_future$map_coeff2 - CS_future$map_pred2 > bet_edge, "KellyMap"] <- sapply(rownames(CS_Result[CS_future$map_coeff2 - CS_future$map_pred2 > bet_edge,]), function(x) kelly(CS_Result, x, "map_coeff2", "map_pred2"))
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "team1"]
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "KellyGun"] <- sapply(rownames(CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge,]), function(x) kelly(CS_Result, x, "gun_coeff1", "gun_pred1"))
CS_Result[CS_future$gun_coeff2 - CS_future$gun_pred2 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff2 - CS_future$gun_pred2 > bet_edge, "team2"]
CS_Result[CS_future$gun_coeff2 - CS_future$gun_pred2 > bet_edge, "KellyGun"] <- sapply(rownames(CS_Result[CS_future$gun_coeff2 - CS_future$gun_pred2 > bet_edge,]), function(x) kelly(CS_Result, x, "gun_coeff2", "gun_pred2"))

CS_Aggr_Map <- CS_Result[!is.na(CS_Result$map), c("team1", "team2", "date", "KellyMap")]
CS_Aggr_Map$bet <- sapply(rownames(CS_Result[!is.na(CS_Result$map),]), function(x) paste("Map:", CS_Result[x, "map"]))
colnames(CS_Aggr_Map) <- c("team1", "team2", "date", "Kelly", "bet")
CS_Aggr_Gun <- CS_Result[!is.na(CS_Result$gun), c("team1", "team2", "date", "KellyGun")]
CS_Aggr_Gun$bet <- sapply(rownames(CS_Result[!is.na(LoL_Result$gun),]), function(x) paste("Gun:", CS_Result[x, "gun"]))
colnames(CS_Aggr_Gun) <- c("team1", "team2", "date", "Kelly", "bet")

CS_Aggr <- rbind(CS_Aggr_Map, CS_Aggr_Gun)
CS_Aggr$game <- "CS"

CS_Result$game <- "CS"
CS_Result <- CS_Result[,c("game","date","team1","team2","map","gun")]
CS_Result <- CS_Result[(!is.na(CS_Result$gun) | !is.na(CS_Result$map)),]

Result <- merge(Result, CS_Result, all=TRUE)

AggrResult <- rbind(D2_Aggr, LoL_Aggr)
AggrResult <- rbind(AggrResult, CS_Aggr)

AggrResult <- AggrResult[c("game", "team1", "team2", "date", "bet", "Kelly")]
AggrResult$Amount <- round(KellyBase * AggrResult$Kelly, 2)

currentWd = getwd()
setwd(OutputPath)
write.table(AggrResult, paste("Kelly.csv", format(Sys.time(), "%Y-%m-%d-%H-%M")), sep="\t", row.names = FALSE)
write.table(Result, paste("Result.csv", format(Sys.time(), "%Y-%m-%d-%H-%M")), sep="\t", row.names = FALSE)
write.csv(D2_FB_WL, "D2_FB_WL.csv")
write.csv(LoL_FB_WL, "LoL_FB_WL.csv")
setwd(currentWd)

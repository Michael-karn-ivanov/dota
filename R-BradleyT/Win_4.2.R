library(readr)
library(BradleyTerry2)
library(reshape)
source("helpers/LoadData.R")
source("helpers/bet.R")

RootPath <- "C:/dev/dota/"
OutputPath <- "C:/dev/dota/results"
Pr = 1.4 #??????? ????????? ????????? ??????????? ? ??????????? ??????, ???? ??????? ??????? ????????? ??????
time_frame = 60*24*60*60 #????????? ??????? ??????? ?????? ? ????????
bet_edge = 0.1 #??????? ??????? ???????????? ? ????????????, ???? ??????? ??????

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
D2_Result$KellyMap1 <- NA
D2_Result$KellyMap2 <- NA
D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "team1"]
D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge, "KellyMap1"] <- sapply(rownames(D2_Result[D2_future$map_coeff1 - D2_future$map_pred1 > bet_edge,]), function(x) kelly(D2_Result, x, "map_coeff1", "map_pred1"))
D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "map"] <- D2_Result[D2_future$map_coeff2 - D2_future$map_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_wl_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_wl_pred2 > bet_edge, "team2"]
D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff1 - D2_future$fb_pred1 > bet_edge, "team1"]
D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "fb"] <- D2_Result[D2_future$fb_coeff2 - D2_future$fb_pred2 > bet_edge, "team2"]
D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff1 - D2_future$kills_pred1 > bet_edge, "team1"]
D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "kills"] <- D2_Result[D2_future$kills_coeff2 - D2_future$kills_pred2 > bet_edge, "team2"]

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
LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff1 - LoL_future$map_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "map"] <- LoL_Result[LoL_future$map_coeff2 - LoL_future$map_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_wl_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_wl_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff1 - LoL_future$fb_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "fb"] <- LoL_Result[LoL_future$fb_coeff2 - LoL_future$fb_pred2 > bet_edge, "team2"]
LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff1 - LoL_future$kills_pred1 > bet_edge, "team1"]
LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "kills"] <- LoL_Result[LoL_future$kills_coeff2 - LoL_future$kills_pred2 > bet_edge, "team2"]

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
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "team1"]
CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "map"] <- CS_Result[CS_future$map_coeff1 - CS_future$map_pred1 > bet_edge, "team2"]
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "team1"]
CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "gun"] <- CS_Result[CS_future$gun_coeff1 - CS_future$gun_pred1 > bet_edge, "team2"]

CS_Result$game <- "CS"
CS_Result <- CS_Result[,c("game","date","team1","team2","map","gun")]
CS_Result <- CS_Result[(!is.na(CS_Result$gun) | !is.na(CS_Result$map)),]

Result <- merge(Result, CS_Result, all=TRUE)

currentWd = getwd()
setwd(OutputPath)
write.table(Result, paste("Result.csv", format(Sys.time(), "%Y-%m-%d-%H-%M")), sep="\t", row.names = FALSE)
write.csv(D2_FB_WL, "D2_FB_WL.csv")
write.csv(LoL_FB_WL, "LoL_FB_WL.csv")
setwd(currentWd)

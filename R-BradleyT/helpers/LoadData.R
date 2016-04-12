LoadDataSet <- function(path) {
  wd <- getwd()
  dataPath <- paste(path, "coeffgrabber New/Data", sep="")
  setwd(dataPath)
  files = list.files(path = dataPath, pattern="*.csv")
  egbPath = paste(dataPath, "!EGB.csv", sep="/")
  EGB <- read.csv(egbPath, header = FALSE, stringsAsFactors=FALSE)
  
  for (i in 1:(length(files)-1)) 
  {
    EGB_temp <- read.csv(files[i], header = FALSE, stringsAsFactors=FALSE)
    EGB <- merge(EGB, EGB_temp, all=TRUE)
  }
  
  EGB <- EGB[EGB$V12 != "draw",]
  current_date_file <- read.csv(files[length(files)], header = FALSE, stringsAsFactors=FALSE)
  EGB <- merge(EGB, current_date_file, all=TRUE)
  
  EGB <- EGB[,c(2,1,11,3,6,7,12,9,4,5,8,10)]
  colnames(EGB) <- c("game", "bet", "series", "date", "team1","team2", "win", "league", "coeff1", "coeff2", "bet_type", "map")
  EGB$date <- as.POSIXct(EGB$date)
  EGB <- EGB[(EGB$date>"2016/03/01"),]
  
  EGB[EGB$team1 == "!Rebels!","team1"] <- "Rebels"
  EGB[EGB$team2 == "!Rebels!","team2"] <- "Rebels"
  EGB[EGB$team1 == "BrooDMotherS","team1"] <- "BroodMothers"
  EGB[EGB$team2 == "BrooDMotherS","team2"] <- "BroodMothers"
  EGB[EGB$team1 == "The Mongolz","team1"] <- "TheMongolz"
  EGB[EGB$team2 == "The Mongolz","team2"] <- "TheMongolz"
  
  setwd(wd)
  return(EGB)
}
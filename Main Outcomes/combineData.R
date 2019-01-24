rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
participant = participant %>% filter(id %in% finalIdLst)
demogData <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/Demographic_recovered_Jun_13_2018.csv")
dmg = demogData %>% filter(participantRSA %in% finalIdLst)
dmg = dmg[!(duplicated(dmg[, c("participantRSA", "session")])),]
#------------------------------------------------------
setwd("/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/")
print(list.files(path = "Data/"))
bbsiq = read.csv("Data/22JanBBSIQ.csv")
DASS_AS = read.csv("Data/22JanDASS_AS.csv")
DASS_DS = read.csv("Data/22JanDASS_DS.csv")
RRScr = read.csv("Data/22JanRR.csv")
OASIS = read.csv("Data/22JanOASIS.csv")
participant = read.csv("Data/22JanParticipants.csv")
#------------------------------------------------------
finalData <- data.frame(matrix(NA, nrow = length(finalIdLst)))
cnt <- 1
for(id in finalIdLst){
  finalData$Id[cnt] <- id
  finalData$birthYear[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$birthYear)
  finalData$date[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$date)
  finalData$education[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$education)
  finalData$employmentStatus[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$employmentStatus)
  finalData$ethnicity[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$ethnicity)
  finalData$gender[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$gender)
  finalData$income[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$income)
  finalData$maritalStatus[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$maritalStatus)
  finalData$race[cnt] <- dmg[which(dmg$participantRSA == id),]$race
  finalData$residenceCountry[cnt] <- as.matrix(dmg[which(dmg$participantRSA == id),]$residenceCountry)
  
  finalData$primeCondition[cnt] <- as.matrix(participant[which(participant$id == id),]$prime)
  finalData$cbmCondition[cnt] <- as.matrix(participant[which(participant$id == id),]$cbmCondition)
  finalData$FIFTY_FIFTYANXIETY[cnt] <- as.matrix(participant[which(participant$id == id),]$FIFTY_FIFTYANXIETY)
  finalData$FIFTY_FIFTYNEUTRAL[cnt] <- as.matrix(participant[which(participant$id == id),]$FIFTY_FIFTYNEUTRAL)
  finalData$NONEANXIETY[cnt] <- as.matrix(participant[which(participant$id == id),]$NONEANXIETY)
  finalData$NEUTRALNEUTRAL[cnt] <- as.matrix(participant[which(participant$id == id),]$NEUTRALNEUTRAL)
  finalData$POSITIVEANXIETY[cnt] <- as.matrix(participant[which(participant$id == id),]$POSITIVEANXIETY)
  finalData$POSITIVENEUTRAL[cnt] <- as.matrix(participant[which(participant$id == id),]$POSITIVENEUTRAL)
  finalData$No.Scenario[cnt] <- as.matrix(participant[which(participant$id == id),]$No.Scenario)
  finalData$FIFTY_FIFTY[cnt] <- as.matrix(participant[which(participant$id == id),]$FIFTY_FIFTY)
  finalData$POSITIVE[cnt] <- as.matrix(participant[which(participant$id == id),]$POSITIVE)
  finalData$NEUTRAL[cnt] <- as.matrix(participant[which(participant$id == id),]$NEUTRAL)
  finalData$ANXIETY[cnt] <- as.matrix(participant[which(participant$id == id),]$ANXIETY)
  
  if(length(row.names(OASIS[which(OASIS$Id == id),])) != 0){
    finalData$OASISScore_PRE[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_PRE)
    finalData$OASISScoreDropout_PRE[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_PRE)
    finalData$OASISScore_SESSION1[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION1)
    finalData$OASISScoreDropout_SESSION1[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION1)
    finalData$OASISScore_SESSION2[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION2)
    finalData$OASISScoreDropout_SESSION2[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION2)
    finalData$OASISScore_SESSION3[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION3)
    finalData$OASISScoreDropout_SESSION3[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION3)
    finalData$OASISScore_SESSION4[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION4)
    finalData$OASISScoreDropout_SESSION4[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION4)
    finalData$OASISScore_SESSION5[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION5)
    finalData$OASISScoreDropout_SESSION5[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION5)
    finalData$OASISScore_SESSION6[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION6)
    finalData$OASISScoreDropout_SESSION6[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION6)
    finalData$OASISScore_SESSION7[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION7)
    finalData$OASISScoreDropout_SESSION7[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION7)
    finalData$OASISScore_SESSION8[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_SESSION8)
    finalData$OASISScoreDropout_SESSION8[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_SESSION8)
    finalData$OASISScore_POST[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScore_POST)
    finalData$OASISScoreDropout_POST[cnt] <- as.matrix(OASIS[which(OASIS$Id == id),]$OASISScoreDropout_POST)
    
  }
  
  if(length(row.names(RRScr[which(RRScr$Id == id),])) != 0){
    finalData$RRNegativeFoil_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoil_PRE)
    finalData$RRPositiveFoil_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoil_PRE)
    finalData$RRNegative_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegative_PRE)
    finalData$RRPositive_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositive_PRE)
    finalData$RRNegativeFoilDropout_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoilDropout_PRE)
    finalData$RRPositiveFoilDropout_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoilDropout_PRE)
    finalData$RRNegativeDropout_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeDropout_PRE)
    finalData$RRPositiveDropout_PRE[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveDropout_PRE)
    finalData$RRNegativeFoil_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoil_SESSION3)
    finalData$RRPositiveFoil_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoil_SESSION3)
    finalData$RRNegative_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegative_SESSION3)
    finalData$RRPositive_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositive_SESSION3)
    finalData$RRNegativeFoilDropout_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoilDropout_SESSION3)
    finalData$RRPositiveFoilDropout_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoilDropout_SESSION3)
    finalData$RRNegativeDropout_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeDropout_SESSION3)
    finalData$RRPositiveDropout_SESSION3[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveDropout_SESSION3)
    finalData$RRNegativeFoil_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoil_SESSION6)
    finalData$RRPositiveFoil_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoil_SESSION6)
    finalData$RRNegative_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegative_SESSION6)
    finalData$RRPositive_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositive_SESSION6)
    finalData$RRNegativeFoilDropout_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeFoilDropout_SESSION6)
    finalData$RRPositiveFoilDropout_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveFoilDropout_SESSION6)
    finalData$RRNegativeDropout_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRNegativeDropout_SESSION6)
    finalData$RRPositiveDropout_SESSION6[cnt] <- as.matrix(RRScr[which(RRScr$Id == id),]$RRPositiveDropout_SESSION6)
  }
  
  if(length(row.names(bbsiq[which(bbsiq$Id == id),])) != 0){
    finalData$negativeBBSIQ_PRE[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQ_PRE)
    finalData$negativeBBSIQDropout_PRE[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQDropout_PRE)
    finalData$negativeBBSIQ_SESSION6[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQ_SESSION6)
    finalData$negativeBBSIQDropout_SESSION6[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQDropout_SESSION6)
    finalData$negativeBBSIQ_SESSION3[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQ_SESSION3)
    finalData$negativeBBSIQDropout_SESSION3[cnt] <- as.matrix(bbsiq[which(bbsiq$Id == id),]$negativeBBSIQDropout_SESSION3)
  }
  
  if(length(row.names(DASS_AS[which(DASS_AS$Id == id),])) != 0){
    finalData$DassAS_ELIGIBLE[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassAS_ELIGIBLE)
    finalData$DassASDropout_ELIGIBLE[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassASDropout_ELIGIBLE)
    finalData$DassAS_SESSION8[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassAS_SESSION8)
    finalData$DassASDropout_SESSION8[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassASDropout_SESSION8)
    finalData$DassAS_POST[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassAS_POST)
    finalData$DassASDropout_POST[cnt] <- as.matrix(DASS_AS[which(DASS_AS$Id == id),]$DassASDropout_POST)
  }
  
  if(length(row.names(DASS_DS[which(DASS_DS$Id == id),])) != 0){
    finalData$DassDS_PRE[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDS_PRE)
    finalData$DassDSDropout_PRE[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDSDropout_PRE)
    finalData$DassDS_SESSION3[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDS_SESSION3)
    finalData$DassDSDropout_SESSION3[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDSDropout_SESSION3)
    finalData$DassDS_SESSION6[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDS_SESSION6)
    finalData$DassDSDropout_SESSION6[cnt] <- as.matrix(DASS_DS[which(DASS_DS$Id == id),]$DassDSDropout_SESSION6)
  }
  
  cnt <- cnt + 1
  
  
}

#------------------------------------------------------
write.csv(finalData, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanFinalData.csv")




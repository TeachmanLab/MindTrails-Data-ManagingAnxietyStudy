rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
## OASIS ----
ImgPrm <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ImageryPrime_recovered_Jun_13_2018.csv") 
ImgPrm = ImgPrm %>% filter(participantRSA %in% finalIdLst)

ImgPrmIds = unique(ImgPrm$participantRSA)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% ImgPrmIds)

ImgPrm$cbmCondition = NA
ImgPrm$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
ImgPrm[which(ImgPrm$participantRSA %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
ImgPrm[which(ImgPrm$participantRSA %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
ImgPrm[which(ImgPrm$participantRSA %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
ImgPrm[which(ImgPrm$participantRSA %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
ImgPrm[which(ImgPrm$participantRSA %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
ImgPrm[which(ImgPrm$participantRSA %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
ImgPrm[which(ImgPrm$participantRSA %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
ImgPrm[which(ImgPrm$participantRSA %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
ImgPrm[which(ImgPrm$participantRSA %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


ImgPrm$condition = NA
ImgPrm[which(ImgPrm$cbmCondition == "FIFTY_FIFTY" & ImgPrm$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
ImgPrm[which(ImgPrm$cbmCondition == "FIFTY_FIFTY" & ImgPrm$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
ImgPrm[which(ImgPrm$cbmCondition == "POSITIVE" & ImgPrm$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
ImgPrm[which(ImgPrm$cbmCondition == "POSITIVE" & ImgPrm$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
ImgPrm[which(ImgPrm$cbmCondition == "No Scenario" & ImgPrm$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
ImgPrm[which(ImgPrm$cbmCondition == "No Scenario" & ImgPrm$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

ImgPrm$condition <- as.factor(ImgPrm$condition)
ImgPrm$primeCondition <- as.factor(ImgPrm$primeCondition, levels = c("NEUTRAL", "ANXIETY"))
ImgPrm$cbmCondition <- as.factor(ImgPrm$cbmCondition, levels = c("No Scenario", "FIFTY_FIFTY", "POSITIVE"))

#------------------------------------------------------
write.csv(ImgPrm, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanImaginaryPrime.csv")


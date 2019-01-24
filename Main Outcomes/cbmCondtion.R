rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
participant = participant %>% filter(id %in% finalIdLst)

levels(participant$cbmCondition) <- c(levels(participant$cbmCondition), "No Scenario")
participant$cbmCondition[participant$cbmCondition == "NEUTRAL"] = "No Scenario"

participant$condition = NA
participant[which(participant$cbmCondition == "FIFTY_FIFTY" & participant$prime == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
participant[which(participant$cbmCondition == "FIFTY_FIFTY" & participant$prime == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
participant[which(participant$cbmCondition == "POSITIVE" & participant$prime == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
participant[which(participant$cbmCondition == "POSITIVE" & participant$prime == "ANXIETY"),]$condition = "POSITIVEANXIETY"
participant[which(participant$cbmCondition == "No Scenario" & participant$prime == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
participant[which(participant$cbmCondition == "No Scenario" & participant$prime == "ANXIETY"),]$condition = "NONEANXIETY"

participant$condition <- as.factor(participant$condition)
participant$cbmCondition <- as.factor(participant$cbmCondition)
participant$prime <- as.factor(participant$prime)

#------------------------------------------------------
for(i in levels(participant$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(participant$condition==i,1,0)
  participant <- cbind(participant,tempVar)
  colnames(participant)[length(participant)] <- tempVarString
}

for(i in c("FIFTY_FIFTY","POSITIVE","No Scenario")){
  tempVarString <- i
  tempVar <- ifelse(participant$cbmCondition==i,1,0)
  participant <- cbind(participant,tempVar)
  colnames(participant)[length(participant)] <- tempVarString
}

for(i in levels(participant$prime)){
  tempVarString <- i
  tempVar <- ifelse(participant$prime==i,1,0)
  participant <- cbind(participant,tempVar)
  colnames(participant)[length(participant)] <- tempVarString
}

#------------------------------------------------------
write.csv(participant, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanParticipants.csv")

rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
## reading OASIS raw data -----
OASIS <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/OA_recovered_Jun_15_2018_FIXED.csv") 
OASIS  = OASIS[!(duplicated(OASIS[, c("participantRSA", "corrected_session")])),]
OASIS = OASIS %>% filter(participantRSA %in% finalIdLst)
slcSession = unique(OASIS$session)
OASIS = OASIS %>% filter(corrected_session %in% slcSession)

#------------------------------------------------------
## selecting rows that prefer not to answer ----
lenCnt <- length(apply(OASIS, 1, function(r) any(r %in% c(555))))
rowNumber <- list()
cnt <- 1
for(i in 1:lenCnt){
  if(apply(OASIS, 1, function(r) any(r %in% c('555')))[i]){
    rowNumber[cnt] <- i
    cnt <- cnt + 1
  }
}
rowNumber <- array(as.numeric(unlist(rowNumber)))

OASISPreferNotAnswer <- OASIS[rowNumber,]
OASIS <- OASIS[-rowNumber, ]
#------------------------------------------------------
pnaIds = unique(OASISPreferNotAnswer$participantRSA)
### calculate OASIS score for those that contain prefer not to answer ----
OASISPreferNotAnswer <- OASISPreferNotAnswer[,c(2,3,4,7,8,10,13)]
OASISnp <- data.frame(matrix(NA, nrow = length(pnaIds)*length(slcSession), ncol = 3))
colnames(OASISnp) <- c('Id','session','OASISScore')
cnt <- 1
for(id in pnaIds){
  sessions <- OASISPreferNotAnswer[which(OASISPreferNotAnswer$participantRSA == id),]$corrected_session
  for(session in sessions){
    OASISnp$Id[cnt] <- id
    OASISnp$session[cnt] <- session 
    if(dim(OASISPreferNotAnswer[which(OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session),])[1] != 0){
      ngValue <- which(OASISPreferNotAnswer[which(OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session),] == 555)
      if(length(ngValue) != 0){
        tmp1 <- OASISPreferNotAnswer[which(OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session),][,-ngValue]
        tmp1 <- tmp1[,-which(names(tmp1) == 'corrected_session' | names(tmp1) == 'participantRSA')]
        if(length(tmp1) != 0){
          if(length(tmp1) > 1)
            OASISnp$OASISScore[cnt] <- sum(as.numeric(tmp1)) + (length(ngValue) * mean(as.numeric(tmp1)))
          else
            OASISnp$OASISScore[cnt] <- as.numeric(tmp1) +  (length(ngValue) * mean(as.numeric(tmp1)))
        }
        else
          OASISnp$OASISScore[cnt] <- NA
      }
      else
        OASISnp$OASISScore[cnt] <- sum(OASISPreferNotAnswer$anxious_freq[OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session],OASISPreferNotAnswer$anxious_sev[OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session],OASISPreferNotAnswer$avoid[OASISPreferNotAnswer$participantRSA == id & OASIS$corrected_session == session],OASIS$interfere[OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session],OASISPreferNotAnswer$interfere_social[OASISPreferNotAnswer$participantRSA == id & OASISPreferNotAnswer$corrected_session == session])
     
    }
    else
      OASISnp$RRNegative[cnt] < NA
    
    
    
    cnt <- cnt + 1
  }
}

OASISnp <- OASISnp[rowSums(is.na(OASISnp)) != ncol(OASISnp),]

#------------------------------------------------------
### calculate OASIS score for those that answer all the questions ----
aOasisIds = unique(OASIS$participantRSA)

OASISNew <- data.frame(matrix(NA, nrow = length(aOasisIds)*length(slcSession), ncol = 3))
colnames(OASISNew) <- c('Id','session','OASISScore')
cnt <- 1
for(id in aOasisIds){
  sessions <- OASIS$corrected_session[OASIS$participantRSA == id]
  for(session in sessions){
    OASISNew$Id[cnt] <- id
    OASISNew$session[cnt] <- session
    OASISNew$OASISScore[cnt] <- sum(OASIS$anxious_freq[OASIS$participantRSA == id & OASIS$corrected_session == session],OASIS$anxious_sev[OASIS$participantRSA == id & OASIS$corrected_session == session],OASIS$avoid[OASIS$participantRSA == id & OASIS$corrected_session == session],OASIS$interfere[OASIS$participantRSA == id & OASIS$corrected_session == session],OASIS$interfere_social[OASIS$participantRSA == id & OASIS$corrected_session == session])
    cnt <- cnt + 1
  }
}

OASISNew = OASISNew[rowSums(is.na(OASISNew)) != ncol(OASISNew), ]

#------------------------------------------------------
## merging OASIS with prefer not to answer OASIS ----
newOASIS <- rbind(OASISNew,OASISnp)

oasisIds = unique(newOASIS$Id)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% oasisIds)

newOASIS$cbmCondition = NA
newOASIS$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
newOASIS[which(newOASIS$Id %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
newOASIS[which(newOASIS$Id %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
newOASIS[which(newOASIS$Id %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
newOASIS[which(newOASIS$Id %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
newOASIS[which(newOASIS$Id %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
newOASIS[which(newOASIS$Id %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
newOASIS[which(newOASIS$Id %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
newOASIS[which(newOASIS$Id %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
newOASIS[which(newOASIS$Id %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


newOASIS$condition = NA
newOASIS[which(newOASIS$cbmCondition == "FIFTY_FIFTY" & newOASIS$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
newOASIS[which(newOASIS$cbmCondition == "FIFTY_FIFTY" & newOASIS$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
newOASIS[which(newOASIS$cbmCondition == "POSITIVE" & newOASIS$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
newOASIS[which(newOASIS$cbmCondition == "POSITIVE" & newOASIS$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
newOASIS[which(newOASIS$cbmCondition == "No Scenario" & newOASIS$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
newOASIS[which(newOASIS$cbmCondition == "No Scenario" & newOASIS$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

newOASIS$condition <- as.factor(newOASIS$condition)
newOASIS$cbmCondition <- as.factor(newOASIS$cbmCondition)
newOASIS$primeCondition <- as.factor(newOASIS$primeCondition)

#------------------------------------------------------
for(i in levels(newOASIS$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(newOASIS$condition==i,1,0)
  newOASIS <- cbind(newOASIS,tempVar)
  colnames(newOASIS)[length(newOASIS)] <- tempVarString
}

for(i in levels(newOASIS$cbmCondition)){
  tempVarString <- i
  tempVar <- ifelse(newOASIS$cbmCondition==i,1,0)
  newOASIS <- cbind(newOASIS,tempVar)
  colnames(newOASIS)[length(newOASIS)] <- tempVarString
}

for(i in levels(newOASIS$primeCondition)){
  tempVarString <- i
  tempVar <- ifelse(newOASIS$primeCondition==i,1,0)
  newOASIS <- cbind(newOASIS,tempVar)
  colnames(newOASIS)[length(newOASIS)] <- tempVarString
}

colnames(newOASIS)

slcScoreClm = c(grep("^OASISScore", colnames(newOASIS)))

for(i in slcScoreClm){
  newOASIS <- cbind(newOASIS,ifelse(!is.na(newOASIS[,i]),1,0))
  colnames(newOASIS)[length(newOASIS)] <- paste(colnames(newOASIS)[i],"Dropout",sep="")
}

slcScoreDropClm = c(grep("^OASISScore", colnames(newOASIS)))

OASISLGCWide <- (reshape(data=newOASIS,
                         idvar="Id",
                         timevar="session",
                         direction="wide",
                         v.names=colnames(newOASIS)[slcScoreDropClm]))
colnames(OASISLGCWide) <- gsub(".","_",colnames(OASISLGCWide),fixed=TRUE)
colnames(OASISLGCWide)
#------------------------------------------------------
write.csv(OASISLGCWide, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanOASIS.csv")

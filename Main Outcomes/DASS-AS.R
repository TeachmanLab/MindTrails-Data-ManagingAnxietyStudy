rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
## reading DassAS raw data -----
DASSAS <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/DASS21_AS_recovered_Jun_13_2018.csv") 
DASSAS <- DASSAS[!(duplicated(DASSAS[, c("participantDAO", "session")])),]
DASSAS = DASSAS %>% filter(participantDAO %in% finalIdLst)

#------------------------------------------------------
## selecting rows that prefer not to answer ----
lenCnt <- length(apply(DASSAS, 1, function(r) any(r %in% c('-1'))))
rowNumber <- list()
cnt <- 1
for(i in 1:lenCnt){
  if(apply(DASSAS, 1, function(r) any(r %in% c('-1')))[i]){
    rowNumber[cnt] <- i
    cnt <- cnt + 1
  }
}

rowNumber <- array(as.numeric(unlist(rowNumber)))

DASSASPreferNotAnswer <- DASSAS[rowNumber,]
DassAS <- DASSAS[-rowNumber, ]
#------------------------------------------------------
DASSASPreferNotAnswer <- DASSASPreferNotAnswer[,c(1,3,4,6,7,9,10,12,13)]
pnDassIds <- unique(DASSASPreferNotAnswer$participantDAO)
DassASnp <- data.frame(matrix(NA, nrow = length(pnDassIds), ncol = 3))
colnames(DassASnp) <- c('Id','session','DassAS')
cnt <- 1
for(id in pnDassIds){
  sessions <- DASSASPreferNotAnswer[which(DASSASPreferNotAnswer$participantDAO == id),]$session
  for(session in sessions){
    DassASnp$Id[cnt] <- id
    DassASnp$session[cnt] <- session 
    if(dim(DASSASPreferNotAnswer[which(DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session),])[1] != 0){
      ngValue <- which(DASSASPreferNotAnswer[which(DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session),] == -1)
      cntNotEligible = 0
      if(length(ngValue) != 0){
        tmp1 <- DASSASPreferNotAnswer[which(DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session),][,-ngValue]
        tmp1 <- tmp1[,-which(names(tmp1) == 'session' | names(tmp1) == 'participantDAO')]
        if(length(tmp1) != 0){
          if(length(tmp1) > 1)
            DassASnp$DassAS[cnt] <- 2*sum(tmp1)
          else
            DassASnp$DassAS[cnt] <- tmp1
        }
        else{
          DassASnp$DassAS[cnt] <- NA
          cntNotEligible = cntNotEligible + 1
          
        }
          
      }
      else
        DassASnp$DassAS[cnt] <- 2*sum(DASSASPreferNotAnswer$breathing[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$dryness[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$heart[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$panic[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$scared[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$trembling[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session],DASSASPreferNotAnswer$worry[DASSASPreferNotAnswer$participantDAO == id & DASSASPreferNotAnswer$session == session])
      
    }
    else
      DassASnp$DassAS[cnt] < NA

    cnt <- cnt + 1
  }
}

DassASnp <- DassASnp[rowSums(is.na(DassASnp)) != ncol(DassASnp),]

#------------------------------------------------------
### calculate OASIS score for those that answer all the questions ----
aDassIds = unique(DassAS$participantDAO)
DassASNew <- data.frame(matrix(NA, nrow = length(aDassIds)*3, ncol = 3))
colnames(DassASNew) <- c('Id','session','DassAS')
cnt <- 1
for(id in aDassIds){
  sessions <- DassAS$session[DassAS$participantDAO == id]
  for(session in sessions){
    DassASNew$Id[cnt] <- id
    DassASNew$session[cnt] <- session
    DassASNew$DassAS[cnt] <- 2*sum(DassAS$breathing[DassAS$participantDAO == id & DassAS$session == session],DassAS$dryness[DassAS$participantDAO == id & DassAS$session == session],DassAS$heart[DassAS$participantDAO == id & DassAS$session == session],DassAS$panic[DassAS$participantDAO == id & DassAS$session == session],DassAS$scared[DassAS$participantDAO == id & DassAS$session == session],DassAS$trembling[DassAS$participantDAO == id & DassAS$session == session],DassAS$worry[DassAS$participantDAO == id & DassAS$session == session])
    cnt <- cnt + 1
  }
}

DassASNew = DassASNew[rowSums(is.na(DassASNew)) != ncol(DassASNew),]

#------------------------------------------------------
## merging OASIS with prefer not to answer OASIS ----
newDASS <- rbind(DassASNew,DassASnp)

dassAsIds = unique(newDASS$Id)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% dassAsIds)

newDASS$cbmCondition = NA
newDASS$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
newDASS[which(newDASS$Id %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
newDASS[which(newDASS$Id %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


newDASS$condition = NA
newDASS[which(newDASS$cbmCondition == "FIFTY_FIFTY" & newDASS$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
newDASS[which(newDASS$cbmCondition == "FIFTY_FIFTY" & newDASS$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
newDASS[which(newDASS$cbmCondition == "POSITIVE" & newDASS$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
newDASS[which(newDASS$cbmCondition == "POSITIVE" & newDASS$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
newDASS[which(newDASS$cbmCondition == "No Scenario" & newDASS$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
newDASS[which(newDASS$cbmCondition == "No Scenario" & newDASS$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

newDASS$condition <- as.factor(newDASS$condition)
newDASS$primeCondition <- as.factor(newDASS$primeCondition, levels = c("NEUTRAL", "ANXIETY"))
newDASS$cbmCondition <- as.factor(newDASS$cbmCondition, levels = c("No Scenario", "FIFTY_FIFTY", "POSITIVE"))


#------------------------------------------------------
for(i in levels(newDASS$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(newDASS$condition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}

for(i in levels(newDASS$cbmCondition)){
  tempVarString <- i
  tempVar <- ifelse(newDASS$cbmCondition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}


for(i in levels(newDASS$primeCondition)){
  tempVarString <- i
  tempVar <- ifelse(newDASS$primeCondition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}

slcScoreClm = c(grep("^DassAS", colnames(newDASS)))
for(i in slcScoreClm){
  newDASS <- cbind(newDASS,ifelse(!is.na(newDASS[,i]),1,0))
  colnames(newDASS)[length(newDASS)] <- paste(colnames(newDASS)[i],"Dropout",sep="")
}

slcScoreDropClm = c(grep("^DassAS", colnames(newDASS)))
DassASLGCWide <- (reshape(data=newDASS,
                          idvar="Id",
                          timevar="session",
                          direction="wide",
                          v.names=colnames(newDASS)[slcScoreDropClm]))
colnames(DassASLGCWide) <- gsub(".","_",colnames(DassASLGCWide),fixed=TRUE)

View(DassASLGCWide)
#------------------------------------------------------
write.csv(DassASLGCWide, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanDASS_AS.csv")




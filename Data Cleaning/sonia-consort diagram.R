#------------------------------------------------------------------------------#
# "DASS AS" table                                                              #
#------------------------------------------------------------------------------#
dassa <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_final-2/Raw Data/DASS21_AS_recovered_Feb_02_2019.csv", header = TRUE)
dassa$participantID <- as.numeric(as.character(dassa$participantDAO))
dassa_have_eligEmpty = filter(dassa, session %in% c("ELIGIBLE", ""))

# No account creation
#------------------------------------------------------------------------------#
dass_have_empty =  filter(dassa_have_eligEmpty, session %in% c(""))
duplicate_nodup_empty = dass_have_empty[!(duplicated(dass_have_empty[, c("sessionId")])),]
dim(duplicate_nodup_empty) #2534
duplicate_nodup_empty$score = 2*rowSums(duplicate_nodup_empty[, c("breathing", "dryness", "heart", "panic",
                                                        "scared", "trembling", "worry")])
duplicate_nodup_empty_right = filter(duplicate_nodup_empty, score > 10) #1820
dim(duplicate_nodup_empty_right)

#2534-1820 = 714 were ineligible

# elligible session
#------------------------------------------------------------------------------#
dass_elig = filter(dassa_have_eligEmpty, session == "ELIGIBLE")
dass_dup_elig = dass_elig[(duplicated(dass_elig[, c("participantID","session")])),] # 109 duplicates
dass_no_dup_elig = dass_elig[!rev(duplicated(rev(dass_elig$participantID))),]
dim(dass_no_dup_elig) #1851

is.na(dass_no_dup_elig) <- dass_no_dup_elig == -1; is.na(dass_no_dup_elig) <- dass_no_dup_elig == -2
dass_no_dup_elig$score = 2*rowSums(dass_no_dup_elig[, c("breathing", "dryness", "heart", "panic",
                                                         "scared", "trembling", "worry")])
# spam account - got the gift card
dass_no_spam = (filter(dass_no_dup_elig, participantID<20 | participantID>420))

dim(dass_no_spam %>% filter(is.na(score)))

dass_no_dup_elig_right = filter(dass_no_spam, score > 10)
dim(dass_no_dup_elig_right)


#------------------------------------------------------------------------------#
# "participant" table                                                              #
#------------------------------------------------------------------------------#
participant <- read.csv("ParticipantExport_recovered_Feb_02_2019.csv", header = TRUE)
part1 <- participant %>% select(participantID = id, everything()) # Create ID variables consistent with all other tables

# Replace column values with TRUE or FALSE
part1 <- mutate(part1, active = ifelse(active=="\001","TRUE","FALSE"))
part1 <- mutate(part1, admin = ifelse(admin=="\001","TRUE","FALSE"))
part1 <- mutate(part1, email_optout = ifelse(email_optout=="\001","TRUE","FALSE"))
part1 <- mutate(part1, increase30 = ifelse(increase30=="\001","TRUE","FALSE"))
part1 <- mutate(part1, over18 = ifelse(over18=="\001","TRUE","FALSE"))


part3 <- filter(part2, participantID<20 | participantID>420) #1524

# There was no variable to identify test accounts, so I manually searched for test IDs in the database 
# and removed accounts associated with PACTlab members or if they had the word "test" in their name/email
test <- c(1,2,4,5,441,450,538,540,578,610,624,718,767,753,775,847,848,926,971,1014,1020:1026,
          1031:1033,1038,1058,1187,1213,1215,1220:1226,1232,1263,1270,1288,1309,1338,1407,
          1486:1488,1490,1499,1500,1608,1631,1740,1767,1817:1819,1831,1899,1900,1968,1971) 

# Remove test accounts
part4 <- part3[!part3$participantID%in%test,] #1496
part5 <- filter(part4, over18=="FALSE") #46 (admin and less 18)
dim(part5)



# "Finalize the participant ID list" 
#------------------------------------------------------------------------------#
dass_no_dup_elig_right = dass_no_dup_elig_right[!dass_no_dup_elig_right$participantID%in%test,] #1415
IDList = (unique(dass_no_dup_elig_right$participantID))


#------------------------------------------------------------------------------#
# "Tasklog" table                                                              #
#------------------------------------------------------------------------------#
tasklog <- read.csv("TaskLog_02_02_2019.csv", header = TRUE)
tasklog$participantID <- tasklog$participantdao_id
length(unique(tasklog$participantID))
tl_dup <- tasklog[(duplicated(tasklog[, c("participantID","session_name","task_name")])),]
tasklog_dass = filter(tasklog, participantID %in% IDList)


tasklog %>% filter(participantID %in% (setdiff(IDList, (unique(tasklog$participantID)))))
length((setdiff(IDList, (unique(tasklog$participantID))))) #147, not starting the pre-treatment

participant %>% filter(id %in% (setdiff(IDList, (unique(tasklog$participantID))))) #in pre session


update_IDList = (setdiff(IDList , (setdiff(IDList, (unique(tasklog$participantID)))))) 

update_taskLog = tasklog %>% filter(participantID %in% update_IDList)

finalEntry = update_taskLog[!rev(duplicated(rev(update_taskLog$participantID))),]

pre_taskLog = (filter(finalEntry, session_name == "PRE"))
preID = unique(pre_taskLog$participantID)
View(tasklog %>% filter(participantID %in% preID))
length(preID) #462, did not finish the pre-treatment

length(unique(finalEntry$participantID))

setdiff((unique(tasklog$participantID)), IDList)





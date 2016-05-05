# Using trial by trial EEG data to bin trials and examine effects of time/fatigue/habituation
# Makes event files for SCAN, can't figure out how to read them in though

# Instead of all this, make new ev2 file with new trigger codes that indicate bins
# 00X = Black eyes
#     001 = bin 1
#     002 = bin 2
#     003 = bin 3
#     ...
# 05X = Black forehead
#     051 = bin 1
#     052 = bin 2
#     053 = bin 3
#     ...
# 10X = White eyes
#     101 = bin 1
#     102 = bin 2
#     103 = bin 3
#     ...
# 15X = White forehead
#     151 = bin 1
#     152 = bin 2
#     153 = bin 3
#     ...

# 200 is not included trial (not correct response or trial at end that's not in a bin)    

# I have my data set up in an RProject, so I don't need to specify working directory
# If you're not using an RProject, then first specify working directory using setwd()
path = "./March 2016/ERP quant data/1 Eval dat/Trial by trial data/"

# Messed up headers for data file so we want to make our own
# Make a .txt file of "pretty" column names manually
# Make sure the electrodes are in the right order
elec = read.delim(paste(path, "electrodes.txt", sep=""), header = F)
elec = as.character(elec$V1) # converts to vector of strings

numSubjects = 51 # number of subjects included in analysis

ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")

condList = c("Black_forehead","Black_eyes","White_forehead","White_eyes")


### Begin loop for each subject
for (k in 1:numSubjects) {
  
  # read in event file
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev.ev2", sep=""), header=FALSE)

  # identify which trials are in each condition
  names(ev2) = ev2names
  
  BFtrials = ev2$Trial[ev2$Trigger == 111|ev2$Trigger == 112] # Black_forehead
  WFtrials = ev2$Trial[ev2$Trigger == 121|ev2$Trigger == 122] # White_forehead
  BEtrials = ev2$Trial[ev2$Trigger == 211|ev2$Trigger == 212] # Black_eyes
  WEtrials = ev2$Trial[ev2$Trigger == 221|ev2$Trigger == 222] # White_eyes

  temp = ev2  
  temp$Condition = NULL
  temp$Condition[temp$Trial %in% BFtrials] = "Black_forehead"
  temp$Condition[temp$Trial %in% BEtrials] = "Black_eyes"
  temp$Condition[temp$Trial %in% WFtrials] = "White_forehead"
  temp$Condition[temp$Trial %in% WEtrials] = "White_eyes"
  
  # For each condition separately, identify trials and bin trials together
  for (y in condList) {
    if (y == "Black_forehead") trialList = BFtrials
    if (y == "White_forehead") trialList = WFtrials
    if (y == "Black_eyes") trialList = BEtrials
    if (y == "White_eyes") trialList = WEtrials
    
    cond.temp = ev2[ev2$Trial %in% trialList & ev2$Resp == 2,]
    numTrials = length(unique(cond.temp$Trial))
    cond.temp$temp.order = rep(1:numTrials)
    cond.temp$Bin = NA
    incTrials = length(rep(1:(numTrials/8), each = 8))
    cond.temp$Bin[1:incTrials] = rep(1:(numTrials/8), each = 8)

    for (l in trialList) {
      temp$Bin[temp$Trial == l & temp$Resp == 2] = cond.temp$Bin[cond.temp$Trial == l]
    }
  }
  
  temp$Trigger2= NULL
  for (i in unique(temp$Bin)){
    temp$Trigger2[temp$Condition == "Black_eyes" & temp$Bin == i] = i # 001 through 015
    temp$Trigger2[temp$Condition == "Black_forehead" & temp$Bin == i] = i+50 # 051 through 065
    temp$Trigger2[temp$Condition == "White_eyes" & temp$Bin == i] = i+100 # 101 through 115
    temp$Trigger2[temp$Condition == "White_forehead" & temp$Bin == i] = i+150 # 151 through 165
  }
  
  temp$Trigger2[is.na(temp$Trigger2)] = 200
  
  rename = temp[c(1, 9, 3:6)]
  write.table(rename, paste(path,"Binned event files/Sub", k, "binnedTriggers.ev2",sep= ""), sep = "\t", row.names = F, col.names = F)
}

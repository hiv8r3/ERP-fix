# Written by Hannah. 4/17/2016
# Takes point data from .eeg files exported from Neuroscan
# Creates smaller files just of epoch of interest for P2
# From those files, calculates average amplitude for P2 for each individual trial to use in trial-by-trial MLM
# and average amplitude for P2 for bins
# Doesn't include trials that are rejected because of artifacts
# Only includes correct trials

# I have my data set up in an RProject, so I don't need to specify working directory
# If you're not using an RProject, then first specify working directory using setwd()
path = "./Analyses with full sample/ERP quant data/2 Cat dat/Trial by trial data/"

# Messed up headers for data file so we want to make our own
# Make a .txt file of "pretty" column names manually
# Make sure the electrodes are in the right order
elec = read.delim(paste(path, "electrodesAvgMast.txt", sep=""), header = F)
elec = as.character(elec$V1) # converts to vector of strings

electrodeList = c("CZ", "CPZ", "PZ", "C3", "C4", "CP3", "CP4") # specify electrodes that you want quantified
numSubjects = 65 # number of subjects included in analysis

beginEpoch = -100 # how many ms before onset does epoch start
endEpoch = 1000 # how many ms after onset does epoch end
lengthEpoch = endEpoch - beginEpoch + 1

# If you want to specify different windows to quantify ERP in for each subject, create excel file 
# separately specifying beginning and end of time window for each subject
subInts = read.delim(paste(path, "subjectQuantIntervals.txt", sep=""), header = T)
# Otherwise, specify interval you want to quantify within
intBegin = NA
intEnd = NA

NumAcceptedTrials = NULL # will become file that has how many trials were accepted for each subject

## Sample filenames:

# path/Cat_Sub1_TBT.dat            # Trial-by-trial point data exported from Scan
# path/Cat_Sub1_REJ.dat            # List of whether trial was rejected or accepted exported from Scan
# path/Cat_Sub1_chan.dat           # Says whether electrodes are bad or skipped, exported from Scan
# path/Event files/Sub1_rev_corrected.ev2     # ev2 file spit out by Scott's EZev2 program, corrected in R script


### Makes smaller file for each subject with point data just for accepted trials and just within quant interval of interest
for (k in c(1:64,66)) {
  
  # read in trial-by-trial point data
  temp = read.delim(paste(path, "Individual files (avg mast)/Cat_Sub", k, "_TBT.dat", sep=""), skip=2, header=FALSE, colClasses = "numeric") 
  names(temp) = elec # replace column names
  temp = temp[,1:length(elec)] # get rid of last NA column
  
  numTrials = nrow(temp)/lengthEpoch # figures out how many trials are in data
  
  # add identifiers
  temp$Subject = k
  temp$Points = rep(1:lengthEpoch, numTrials)
  temp$Time = rep(beginEpoch:endEpoch, numTrials)
  temp$Trial = rep(1:numTrials, each = lengthEpoch)
  
  # set quant interval for each subject
  intBegin = subInts$Begin[subInts$Subject == k]
  intEnd = subInts$End[subInts$Subject == k]
  intLength = intEnd-intBegin + 1
  
  # select only points of interest
  temp = temp[temp$Time >= intBegin & temp$Time <= intEnd,]
  
  # read in file of rejected trials
  rej = read.delim(paste(path, "Individual files (avg mast)/Cat_Sub", k, "_REJ.dat", sep=""), header=FALSE, colClasses = "numeric") 
  # read in event file to add whether response was correct, condition, etc.
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev_corrected.ev2", sep=""), header=FALSE)
  
  for (i in 1:numTrials) {
    # add rejection status from Scan: 0 = reject, 1 = accept
    temp$Rejected[temp$Trial == i] = rej[i,1] 
    # add whether response was correct:  2 = correct, 1 = incorrect, 3 = timeout
    temp$Correct[temp$Trial == i] = ev2$V3[ev2$V1 == i] 
    }
  
  # Calculate the number of trials that were correct (if there is a correct response), rejected due to
  # EEG artifacts and how many trials in total that were accepted (correct and no artifacts)
  temp.select = temp[temp$Rejected == 1 & temp$Correct == 2,] # selects subset of just correct, accepted trials
  numAccepted = length(unique(temp.select$Trial))
  numAccTemp = data.frame(Subject = k,
                          numTrials = numTrials, # number of trials recorded total
                          numArtifact = 512 - sum(rej), # number of trials that were rejected because of EEG artifacts
                          numIncorrect = length(unique(temp$Trial[!(temp$Correct == 2)])),
                          numAccept = numAccepted)
  
  NumAcceptedTrials = rbind(NumAcceptedTrials, numAccTemp)

  # write file (only correct trials that haven't been rejected for artifacts, only quant interval of interest)
  write.table(temp.select, paste(path, "Individual files (avg mast)/Cat_Sub", k, "_TBT_acceptedTrials_P2quantInterval.txt", sep=""),
              sep="\t", row.names = F)
}

write.table(NumAcceptedTrials, paste(path,"NumberOfAcceptedTrials.txt", sep=""), sep = "\t", row.names = F)



#### Takes smaller files and calculates average amplitude in interval of interest. Adds value for each trial to a new data set
QuantDat = NULL # will become long form with following columns: Subject, Trial, Electrode, MeanAmp

for (k in c(1:64,66)) {

  # read in smaller trial-by-trial point data files
  temp2 = read.delim(paste(path, "Individual files (avg mast)/Cat_Sub", k, "_TBT_acceptedTrials_P2quantInterval.txt", sep=""), header=T, colClasses = "numeric") 
  
  # Now for each trial, calculate average amplitude in time window of interest and add value for each trial to new data set
  # create data set to put average amp for each trial
  avgTemp = data.frame(Subject = k,
                       Trial = rep(unique(temp2$Trial), each = length(electrodeList)),
                       Electrode = rep(electrodeList, length(unique(temp2$Trial)))) 
  
  # Begin loop to quantify interval for each trial, for each electrode
  for (i in unique(avgTemp$Trial)) {  # go one trial at a time
    for (l in electrodeList) { # one electrode at a time
      avg = temp2[temp2$Trial == i, l] %>% 
        mean()
      avgTemp$MeanAmp[avgTemp$Trial == i & avgTemp$Electrode == l] = avg
    }
  }
  
  # Take out data if electrode is bad
  badElec = read.delim(paste(path, "Individual files (avg mast)/Cat_Sub", k, "_chan.log", sep=""), header = T)
  for (l in electrodeList) {
    if (badElec$Bad[badElec$Channel == l] == 1){
      avgTemp$MeanAmp[avgTemp$Electrode == l] = NA
    } else if (badElec$Skip[badElec$Channel == l] == 1) {
      avgTemp$MeanAmp[avgTemp$Electrode == l] = NA
    }
  }
  QuantDat = rbind(QuantDat, avgTemp)
}
 
# Note: this does not contain information about which condition each trial is. 
# In order to get that, you'll need to use the ev2 file to get that information and pair it up
# with this file by trial. 

QuantDatCond = NULL # will become data frame with all data plus conditions
ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")

for (k in unique(QuantDat$Subject)) {
  temp = subset(QuantDat, Subject == k)
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev_corrected.ev2", sep=""), header=FALSE)
  names(ev2) = ev2names
  
  BFtrials = ev2$Trial[ev2$Trigger == 55]
  temp$Condition[temp$Trial %in% BFtrials] = "Black_forehead"
  
  WFtrials = ev2$Trial[ev2$Trigger == 56]
  temp$Condition[temp$Trial %in% WFtrials] = "White_forehead"
  
  BEtrials = ev2$Trial[ev2$Trigger == 65]
  temp$Condition[temp$Trial %in% BEtrials] = "Black_eyes"
  
  WEtrials = ev2$Trial[ev2$Trigger == 66]
  temp$Condition[temp$Trial %in% WEtrials] = "White_eyes"

  
  temp$Race = NA
  temp$Race[grep("Black", temp$Condition)] = "Black"
  temp$Race[grep("White", temp$Condition)] = "White"

  temp$Fix = NA
  temp$Fix[grep("forehead", temp$Condition)] = "forehead"
  temp$Fix[grep("eyes", temp$Condition)] = "eyes"
  
  QuantDatCond = rbind(QuantDatCond, temp)
}

write.table(QuantDatCond, paste(path,"AllSubs_Cat_TBTaverages_P2_meanAmp.txt", sep=""), sep = "\t", row.names = F)

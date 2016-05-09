# Using trial by trial EEG data to bin trials and examine effects of time/fatigue/habituation
require(dplyr)

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

electrodeList = c("CZ", "CPZ", "PZ") # specify electrodes that you want quantified
numSubjects = 51 # number of subjects included in analysis

beginEpoch = -100 # how many ms before onset does epoch start
endEpoch = 1000 # how many ms after onset does epoch end
lengthEpoch = endEpoch - beginEpoch + 1

# If you want to specify different windows to quantify ERP in for each subject, create excel file 
# separately specifying beginning and end of time window for each subject
subInts = read.delim(paste(path, "subjectQuantIntervals.txt", sep=""), header = T)
# Otherwise, specify interval you want to quantify within
#intBegin = NA
#intEnd = NA

numTrialsTemp = NA
dat.all = NULL # put dat for each sub together
BinningInfoTotal = NULL # will be text file with number of trials and bins per condition for each subject

### Begin loop for each subject
for (k in 1:numSubjects) {
  
  # Need to reset at the beginning of each subject
  dat.sub = NULL # will become data set with a calculated mean amplitude in interval of 
  # interest for each electrode, for each bin, for each condition
  BinningInfoSub = NULL # will be text file with number of trials and bins per condition for each subject
  
  # read in trial-by-trial point data
  temp = read.delim(paste(path, "Eval_Sub", k, "_TBT.dat", sep=""), skip=2, header=FALSE, colClasses = "numeric") 
  names(temp) = elec # replace column names
  temp = temp[,1:length(elec)] # get rid of last NA column
  
  numTrials = nrow(temp)/lengthEpoch # figures out how many trials are in data
  
  # add identifiers
  temp$Subject = k
  temp$Points = rep(1:lengthEpoch, numTrials)
  temp$Time = rep(beginEpoch:endEpoch, numTrials)
  temp$Trial = rep(1:numTrials, each = lengthEpoch)
  
  # restrict data to epoch of interest so that data files aren't as huge
  intBegin = subInts$Begin[subInts$Subject == k]
  intEnd = subInts$End[subInts$Subject == k]
  intLength = intEnd - intBegin + 1
  temp = temp[temp$Time >= intBegin & temp$Time <= intEnd,]
  
  # read in file of rejected trials
  rej = read.delim(paste(path, "Eval_Sub", k, "_REJ.dat", sep=""), header=FALSE, colClasses = "numeric") 
  # read in event file to add whether response was correct, condition, etc.
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev.ev2", sep=""), header=FALSE)
  names(ev2) = ev2names
  
  for (i in unique(ev2$Trial)) {
    # add rejection status from Scan: 0 = reject, 1 = accept
    temp$Rejected[temp$Trial == i] = rej[i,1] 
    # add whether response was correct:  2 = correct, 1 = incorrect, 3 = timeout
    temp$Correct[temp$Trial == i] = ev2$Resp[ev2$Trial == i] 
  }
  
  # restrict temp to trials where response is correct and trial is not rejected
  temp.select = temp[temp$Correct == 2 & temp$Rejected == 1,]
  
  # Now start binning trials
  
  # identify which trials are in each condition
  BFtrials = ev2$Trial[ev2$Trigger == 111|ev2$Trigger == 112] # Black_forehead
  WFtrials = ev2$Trial[ev2$Trigger == 121|ev2$Trigger == 122] # White_forehead
  BEtrials = ev2$Trial[ev2$Trigger == 211|ev2$Trigger == 212] # Black_eyes
  WEtrials = ev2$Trial[ev2$Trigger == 221|ev2$Trigger == 222] # White_eyes

  # For each condition separately, identify trials and bin trials together
  for (y in condList) {
    if (y == "Black_forehead") trialList = BFtrials
    if (y == "White_forehead") trialList = WFtrials
    if (y == "Black_eyes") trialList = BEtrials
    if (y == "White_eyes") trialList = WEtrials
    
    cond.temp = temp.select[temp.select$Trial %in% trialList,]
    numTrialsCond = length(unique(cond.temp$Trial))
    
    cond.temp$temp.order = rep(1:numTrialsCond, each = intLength)
    binnedTrials = length(rep(1:(numTrialsCond/8), each = 8))
    numBins = binnedTrials/8
    
    cond.temp$Bin = NA
    for (p in 1:numBins) {
      q = p*8
      cond.temp$Bin[cond.temp$temp.order <= q & cond.temp$temp.order >= (q-7)] = p
    }

    # write info about number of trials in each condition, how many bins, etc. to fil
    numTrialsTemp = data.frame(Subject = k,
                               numTotalTrials = numTrials,
                               numCondTrials = numTrialsCond, # number of trials accepted in each condition
                               Condition = y,
                               numBins = numBins)
    
    BinningInfoSub = rbind(BinningInfoSub, numTrialsTemp)
    
    # Make average for each bin (in condition loop, so this is only for one condition)
    avgTemp = data.frame(Subject = k,
                         Bin = rep(1:numBins, each = length(electrodeList)),
                         Electrode = rep(electrodeList, numBins),
                         Condition = y) 
    
    for (i in 1:numBins) {  # go one bin at a time
      for (l in electrodeList) { # one electrode at a time
        # identify trials in bin
        binTemp = cond.temp[cond.temp$Bin == i, c(l,"Time","Trial")]
        # create average waveform (averaging across trials in bin for each time point)
        avgDat = data.frame(Time = unique(binTemp$Time),
                            Amp = NA)
        for (j in intBegin:intEnd){
          avgDat$Amp[avgDat$Time == j] = mean(binTemp[binTemp$Time == j,1], na.rm = T)
        }
        avgAmpBin = mean(avgDat$Amp, na.rm = T)
        avgTemp$MeanAmp[avgTemp$Bin == i & avgTemp$Electrode == l] = avgAmpBin
      }
    }
    dat.sub = rbind(dat.sub, avgTemp)
  }
  dat.all = rbind(dat.all, dat.sub)
  BinningInfoTotal = rbind(BinningInfoTotal, BinningInfoSub)
}


# Add separate race and fix columns
dat.all$Race = NA
dat.all$Race[grep("Black", dat.all$Condition)] = "Black"
dat.all$Race[grep("White", dat.all$Condition)] = "White"

dat.all$Fix = NA
dat.all$Fix[grep("forehead", dat.all$Condition)] = "forehead"
dat.all$Fix[grep("eyes", dat.all$Condition)] = "eyes"


write.table(dat.all, paste(path, "Binned Data/BinnedQuantifiedData.txt", sep = ""),sep = "\t", row.names=F)
write.table(BinningInfoTotal, paste(path, "Binned Data/BinningInfo.txt", sep = ""),sep = "\t", row.names=F)

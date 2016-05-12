# Using trial by trial EEG data to bin trials and examine effects of time/fatigue/habituation
require(dplyr)

# I have my data set up in an RProject, so I don't need to specify working directory
# If you're not using an RProject, then first specify working directory using setwd()
path = "./Analyses with full sample/ERP quant data/1 Eval dat/"
TBTfolder = "Trial by trial data/Individual files (avg mast)/"

ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")

condList = c("Black_forehead","Black_eyes","White_forehead","White_eyes")

electrodeList = c("CZ", "CPZ", "PZ", "C3", "C4", "CP3", "CP4") # specify electrodes that you want quantified
numSubjects = 65 # number of subjects included in analysis

numTrialsTemp = NA
dat.all = NULL # put dat for each sub together
BinningInfoTotal = NULL # will be text file with number of trials and bins per condition for each subject

### Begin loop for each subject
for (k in c(1:64,66)) {
  
  # Need to reset at the beginning of each subject
  dat.sub = NULL # will become data set with a calculated mean amplitude in interval of 
  # interest for each electrode, for each bin, for each condition
  BinningInfoSub = NULL # will be text file with number of trials and bins per condition for each subject
  
  # read in trial-by-trial point data (smaller file with just points within interval of interest)
  temp = read.delim(paste(path, TBTfolder, "Eval_Sub", k, "_TBT_acceptedTrials_P2quantInterval.txt", sep=""), header=T, colClasses = "numeric") 

  numTrials = length(unique(temp$Trial)) # figures out how many accepted trials for that subject
  intBegin = min(temp$Time)
  intEnd = max(temp$Time)
  intLength = intEnd - intBegin + 1 # determine number of time points in each trial

  # read in ev2 file
  ev2 = read.delim(paste(path, "/Trial by trial data/Event files/Sub", k, "_rev.ev2", sep=""), header=FALSE)
  names(ev2) = ev2names
  
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
    
    cond.temp = temp[temp$Trial %in% trialList,]
    numTrialsCond = length(unique(cond.temp$Trial))
    
    cond.temp$temp.order = rep(1:numTrialsCond, each = intLength)
    binnedTrials = length(rep(1:(numTrialsCond/8), each = 8)) # max number of trials that divides by 8 evenly
    numBins = binnedTrials/8 # number of bins in condition
    
    cond.temp$Bin = NA # put trials in bin
    # repeat for each bin. Puts trial 1-8 in bin 1, 9-16 in bin 2, etc.
    for (p in 1:numBins) {
      q = p*8 # q is upperbound (trial num) for each bin
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
        # create average waveform (averaging across trials in bin for each time point)- only for quant interval
        avgWaveform = data.frame(Time = unique(binTemp$Time),
                                 Amp = NA)
        for (j in intBegin:intEnd){
          avgWaveform$Amp[avgWaveform$Time == j] = mean(binTemp[binTemp$Time == j,1], na.rm = T)
        }
        # quantify waveform
        avgAmpBin = mean(avgWaveform$Amp, na.rm = T)
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


write.table(dat.all, paste(path, "Binned Data/AllSubs_BinnedAverages_P2.txt", sep = ""),sep = "\t", row.names=F)
write.table(BinningInfoTotal, paste(path, "Binned Data/BinningInfo_P2.txt", sep = ""),sep = "\t", row.names=F)

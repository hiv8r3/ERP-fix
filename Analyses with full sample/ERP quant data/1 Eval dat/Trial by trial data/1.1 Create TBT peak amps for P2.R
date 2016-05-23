#### Takes smaller files and calculates peak amplitude in interval of interest. Adds value for each trial to a new data set
QuantDat = NULL # will become long form with following columns: Subject, Trial, Electrode, MeanAmp
path = "./Analyses with full sample/ERP quant data/1 Eval dat/Trial by trial data/"
electrodeList = c("CZ", "CPZ", "PZ", "C3", "C4", "CP3", "CP4") # specify electrodes that you want quantified

for (k in c(1:64,66)) {
  
  # read in smaller trial-by-trial point data files
  temp2 = read.delim(paste(path, "Individual files (avg mast)/Eval_Sub", k, "_TBT_acceptedTrials_P2quantInterval.txt", sep=""), header=T, colClasses = "numeric") 
  
  # Now for each trial, calculate average amplitude in time window of interest and add value for each trial to new data set
  # create data set to put average amp for each trial
  peakTemp = data.frame(Subject = k,
                       Trial = rep(unique(temp2$Trial), each = length(electrodeList)),
                       Electrode = rep(electrodeList, length(unique(temp2$Trial)))) 
  
  # Begin loop to quantify interval for each trial, for each electrode
  for (i in unique(peakTemp$Trial)) {  # go one trial at a time
    for (l in electrodeList) { # one electrode at a time
      peak = temp2[temp2$Trial == i, l] %>% 
        max()
      peakTemp$PeakAmp[peakTemp$Trial == i & peakTemp$Electrode == l] = peak
    }
  }
  
  # Take out data if electrode is bad
  badElec = read.delim(paste(path, "Individual files (avg mast)/Eval_Sub", k, "_chan.log", sep=""), header = T)
  for (l in electrodeList) {
    if (badElec$Bad[badElec$Channel == l] == 1){
      peakTemp$PeakAmp[peakTemp$Electrode == l] = NA
    } else if (badElec$Skip[badElec$Channel == l] == 1) {
      peakTemp$PeakAmp[peakTemp$Electrode == l] = NA
    }
  }
  QuantDat = rbind(QuantDat, peakTemp)
}

# Note: this does not contain information about which condition each trial is. 
# In order to get that, you'll need to use the ev2 file to get that information and pair it up
# with this file by trial. 

QuantDatCond = NULL # will become data frame with all data plus conditions
ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")

for (k in unique(QuantDat$Subject)) {
  temp = subset(QuantDat, Subject == k)
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev.ev2", sep=""), header=FALSE)
  names(ev2) = ev2names
  
  BPFtrials = ev2$Trial[ev2$Trigger == 111]
  temp$Condition[temp$Trial %in% BPFtrials] = "Black_positive_forehead"
  
  WPFtrials = ev2$Trial[ev2$Trigger == 121]
  temp$Condition[temp$Trial %in% WPFtrials] = "White_positive_forehead"
  
  BNFtrials = ev2$Trial[ev2$Trigger == 112]
  temp$Condition[temp$Trial %in% BNFtrials] = "Black_negative_forehead"
  
  WNFtrials = ev2$Trial[ev2$Trigger == 122]
  temp$Condition[temp$Trial %in% WNFtrials] = "White_negative_forehead"
  
  BPEtrials = ev2$Trial[ev2$Trigger == 211]
  temp$Condition[temp$Trial %in% BPEtrials] = "Black_positive_eyes"
  
  WPEtrials = ev2$Trial[ev2$Trigger == 221]
  temp$Condition[temp$Trial %in% WPEtrials] = "White_positive_eyes"
  
  BNEtrials = ev2$Trial[ev2$Trigger == 212]
  temp$Condition[temp$Trial %in% BNEtrials] = "Black_negative_eyes"
  
  WNEtrials = ev2$Trial[ev2$Trigger == 222]
  temp$Condition[temp$Trial %in% WNEtrials] = "White_negative_eyes"
  
  temp$Race = NA
  temp$Race[grep("Black", temp$Condition)] = "Black"
  temp$Race[grep("White", temp$Condition)] = "White"
  
  temp$WordVal = NA
  temp$WordVal[grep("positive", temp$Condition)] = "positive"
  temp$WordVal[grep("negative", temp$Condition)] = "negative"
  
  temp$Fix = NA
  temp$Fix[grep("forehead", temp$Condition)] = "forehead"
  temp$Fix[grep("eyes", temp$Condition)] = "eyes"
  
  QuantDatCond = rbind(QuantDatCond, temp)
}

write.table(QuantDatCond, paste(path,"AllSubs_TBTaverages_P2_peakAmp.txt", sep=""), sep = "\t", row.names = F)

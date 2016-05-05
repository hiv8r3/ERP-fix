# check to make sure that doing averages in R from trial by trial data will match average data from Scan
# averages calculated in R from TBT matches averages quantified by Scan
# mean difference is <.05, >.001

elec = read.delim(paste(path, "electrodes.txt", sep=""), header = F)
elec = as.character(elec$V1) # converts to vector of strings
numSubjects = 51 # number of subjects included in analysis

beginEpoch = -100 # how many ms before onset does epoch start
endEpoch = 1000 # how many ms after onset does epoch end
lengthEpoch = endEpoch - beginEpoch + 1

# If you want to specify different windows to quantify ERP in for each subject, create excel file 
# separately specifying beginning and end of time window for each subject
subInts = read.delim(paste(path, "subjectQuantIntervals.txt", sep=""), header = T)

ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")

condList = c("Black_forehead","Black_eyes","White_forehead","White_eyes")

indivAverages = NULL # will become dataset with separate quantified averages for each condition for each subject

for (k in 1:numSubjects) {
  
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
  
  # read in file of rejected trials
  rej = read.delim(paste(path, "Eval_Sub", k, "_REJ.dat", sep=""), header=FALSE, colClasses = "numeric") 
  # read in event file to add whether response was correct, condition, etc.
  ev2 = read.delim(paste(path, "/Event files/Sub", k, "_rev.ev2", sep=""), header=FALSE)
  
  for (i in 1:numTrials) {
    # add rejection status from Scan: 0 = reject, 1 = accept
    temp$Rejected[temp$Trial == i] = rej[i,1] 
    # add whether response was correct:  2 = correct, 1 = incorrect, 3 = timeout
    temp$Correct[temp$Trial == i] = ev2$V3[ev2$V1 == i] 
  }
  
  temp.select = temp[temp$Rejected == 1 & temp$Correct == 2,] # selects subset of just correct, accepted trials

  # set quant interval for each subject
  intBegin = subInts$Begin[subInts$Subject == k]
  intEnd = subInts$End[subInts$Subject == k]
  
  select.time = subset(temp.select, Time >= intBegin & Time <= intEnd)
  
  # Add condition information
  names(ev2) = ev2names
  
  BFtrials = ev2$Trial[ev2$Trigger == 111|ev2$Trigger == 112]
  select.time$Condition[select.time$Trial %in% BFtrials] = "Black_forehead"

  WFtrials = ev2$Trial[ev2$Trigger == 121|ev2$Trigger == 122]
  select.time$Condition[select.time$Trial %in% WFtrials] = "White_forehead"

  BEtrials = ev2$Trial[ev2$Trigger == 211|ev2$Trigger == 212]
  select.time$Condition[select.time$Trial %in% BEtrials] = "Black_eyes"
  
  WEtrials = ev2$Trial[ev2$Trigger == 221|ev2$Trigger == 222]
  select.time$Condition[select.time$Trial %in% WEtrials] = "White_eyes"
  
  indivAvg = NULL
  for (y in condList) {
    temp = data.frame(Subject = k,
                      Time = intBegin:intEnd,
                      CZ.avg = NA,
                      CPZ.avg = NA,
                      PZ.avg = NA)
    for (b in unique(select.time$Time)) {
      temp$CZ.avg[temp$Time == b] = mean(select.time$CZ[select.time$Time == b & select.time$Condition == y], na.rm = T)
      temp$CPZ.avg[temp$Time == b] = mean(select.time$CPZ[select.time$Time == b & select.time$Condition == y], na.rm = T)
      temp$PZ.avg[temp$Time == b] = mean(select.time$PZ[select.time$Time == b & select.time$Condition == y], na.rm = T)
    }
    temp = summarise_each(temp, funs(mean))
    temp$Condition = y
    indivAvg = rbind(indivAvg, temp)
  } 
  indivAverages = rbind(indivAverages, indivAvg)
}  




# import quantified averages from Scan
scandat = read.delim("./March 2016/ERP quant data/1 Eval dat/P2quant_AvgAmp_51subsAggregated.txt")

elec = c("CZ", "CPZ", "PZ")
scandat = scandat[scandat$Electrode %in% elec,]

scanCZ = subset(scandat, Electrode == "CZ")
mean(scanCZ$AvgAmp[scanCZ$Condition == "Black_eyes"] - indivAverages$CZ.avg[indivAverages$Condition == "Black_eyes"])
mean(scanCZ$AvgAmp[scanCZ$Condition == "Black_fore"] - indivAverages$CZ.avg[indivAverages$Condition == "Black_forehead"])
mean(scanCZ$AvgAmp[scanCZ$Condition == "White_eyes"] - indivAverages$CZ.avg[indivAverages$Condition == "White_eyes"])
mean(scanCZ$AvgAmp[scanCZ$Condition == "White_fore"] - indivAverages$CZ.avg[indivAverages$Condition == "White_forehead"])


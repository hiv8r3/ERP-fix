require(magrittr)
require(dplyr)

# Amplitude in interval of interest (peak or avg amp) already quantified by scan
# for cat task


##### Mean amp for P2

# Just reading in each file for each subject and putting them together into one file with all subjects
P2dat = read.delim("./Analyses with full sample/ERP quant data/2 Cat dat/Individual averages/Individual quant files/Sub1_P2quant_avgamp.dat", header = T, sep = "\t")

# add column for trial condition
condList = rep("Black_eyes", 24) %>%
  append(rep("Black_fore", 24)) %>%
  append(rep("White_eyes", 24)) %>%
  append(rep("White_fore", 24))
P2dat$Condition = condList

# add column for subject number
P2dat$Subject = 1

# rename average amplitude column so there's no problem rbinding
names(P2dat)[3] = "AvgAmp"

# append the rest of the subjects to same data file using a loop
for (i in c(2:64,66)) {
  temp = read.delim(paste("./Analyses with full sample/ERP quant data/2 Cat dat/Individual averages/Individual quant files/Sub", i, "_P2quant_avgamp.dat", sep = ""), 
                    header = T, sep = "\t")
  temp$Condition = condList
  temp$Subject = i
  names(temp)[3] = "AvgAmp"
  P2dat = rbind(P2dat, temp)
}

P2dat = rename(P2dat, Electrode = Channel) %>%
  select(-Sweep.Number)

# add column specifying race of prime
P2dat$faceRace = NA
P2dat$faceRace[grep("Black", P2dat$Condition)] = "Black"
P2dat$faceRace[grep("White", P2dat$Condition)] = "White"

# add column specifying fixation area
P2dat$FixArea = NA
P2dat$FixArea[grep("eyes", P2dat$Condition)] = "eyes"
P2dat$FixArea[grep("fore", P2dat$Condition)] = "fore"

write.table(P2dat, "./Analyses with full sample/ERP quant data/2 Cat dat/Individual averages/AllSubs_Cat_IndivAverages_P2_meanAmp.txt", row.names = F, sep = "\t")





# #### do the same for N170 data 
# 
# N170dat = read.delim("./Analyses with full sample/ERP quant data/1 Eval dat/Individual averages/Individual quant files/Sub1_N170quant_peak.dat", header = T, sep = "\t")
# N170dat$Subject = 1
# 
# # append the rest of the subjects to same data file using a loop
# for (i in c(2:64,66)) {
#   temp = read.delim(paste("./Analyses with full sample/ERP quant data/1 Eval dat/Individual averages/Individual quant files/Sub", i, "_N170quant_peak.dat", sep = ""), 
#                     header = T, sep = "\t")
#   temp$Subject = i
#   N170dat = rbind(N170dat, temp)
# }
# 
# # add column specifying race of prime
# N170dat$faceRace = NA
# N170dat$faceRace[grep("Black", N170dat$Source.File)] = "Black"
# N170dat$faceRace[grep("White", N170dat$Source.File)] = "White"
# 
# # add column specifying fixation area
# N170dat$FixArea = NA
# N170dat$FixArea[grep("eyes", N170dat$Source.File)] = "eyes"
# N170dat$FixArea[grep("fore", N170dat$Source.File)] = "fore"
# 
# N170dat = rename(N170dat, Electrode = Channel) %>%
#   rename(Peak = Amplitude)
# 
# # add column for hemisphere
# N170dat$Hemisphere[N170dat$Electrode == "TP8"] = "right"
# N170dat$Hemisphere[N170dat$Electrode == "P8/T6"] = "right"
# N170dat$Hemisphere[N170dat$Electrode == "TP7"] = "left"
# N170dat$Hemisphere[N170dat$Electrode == "P7/T5"] = "left"
# 
# # add column for front/back
# N170dat$Coronal[N170dat$Electrode == "TP8"] = "anterior"
# N170dat$Coronal[N170dat$Electrode == "P8/T6"] = "posterior"
# N170dat$Coronal[N170dat$Electrode == "TP7"] = "anterior"
# N170dat$Coronal[N170dat$Electrode == "P7/T5"] = "posterior"
# 
# write.table(N170dat, "./Analyses with full sample/ERP quant data/1 Eval dat/Individual averages/AllSubs_IndivAverages_N170_peakAmp.txt", row.names = F, sep = "\t")

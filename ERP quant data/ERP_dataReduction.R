require(magrittr)
require(dplyr)

# read in individual file
P2dat = read.delim("ERP quant data/Individual files/Sub1_P2quant_avgamp.dat", header = T, sep = "\t")

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
for (i in 2:51) {
  temp = read.delim(paste("ERP quant data/Individual files/Sub", i, "_P2quant_avgamp.dat", sep = ""), 
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

write.table(P2dat, "ERP quant data/P2quant_AvgAmp_51subsAggregated.txt", row.names = F, sep = "\t")

#### do the same for N170 data 

N170dat = read.delim("ERP quant data/Individual files/Sub1_N170quant_peak.dat", header = T, sep = "\t")
N170dat$Subject = 1

# append the rest of the subjects to same data file using a loop
for (i in 2:51) {
  temp = read.delim(paste("ERP quant data/Individual files/Sub", i, "_N170quant_peak.dat", sep = ""), 
                    header = T, sep = "\t")
  temp$Subject = i
  N170dat = rbind(N170dat, temp)
}

# add column specifying race of prime
N170dat$faceRace = NA
N170dat$faceRace[grep("Black", N170dat$Source.File)] = "Black"
N170dat$faceRace[grep("White", N170dat$Source.File)] = "White"

# add column specifying fixation area
N170dat$FixArea = NA
N170dat$FixArea[grep("eyes", N170dat$Source.File)] = "eyes"
N170dat$FixArea[grep("fore", N170dat$Source.File)] = "fore"

hist(N170dat$Latency)

N170dat = rename(N170dat, Electrode = Channel) %>%
  rename(Peak = Amplitude)

# add column for hemisphere
datN170$Hemisphere[datN170$Electrode == "TP8"] = "right"
datN170$Hemisphere[datN170$Electrode == "P8/T6"] = "right"
datN170$Hemisphere[datN170$Electrode == "TP7"] = "left"
datN170$Hemisphere[datN170$Electrode == "P7/T5"] = "left"


write.table(datN170, "ERP quant data/N170quant_Peak_51subsAggregated.txt", row.names = F, sep = "\t")

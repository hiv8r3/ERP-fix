require(magrittr)
require(dplyr)
require(tidyr)

path = "./PCA/1 Eval task/A- Temporo-spatial PCA (noEOG)/2 Factor waveforms separated by subject/EvalTempSpatial_noEOG_2SF_"

# for electrode names
ced = read.delim("./PCA/1 Eval task/Bartholow_CED_v3_ERP-fix.txt", header = T) # ced used in EP toolkit (excludes EOG and nose but includes mastoid references)
factorelec = as.character(ced$labels)

condList = c("BE", "BF", "WE", "WF")
subList = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:64, 66)


# VT-1 --------------------------------------------------------------------

# read in data for temporal component TF10_SF1
VT1dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF10SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT1dat = rbind(VT1dat, temp)
  }
}
VT1dat$Subject = as.integer(VT1dat$Subject)

# select electrodes of interest, time of interest (80-140ms)
VT1wide = VT1dat[VT1dat$Time >= 80 & VT1dat$Time <= 140,]
tp = VT1wide[,1:22] %>% # figure out what electrodes to use
  summarise_each(funs(min)) %>% 
  sort()
plot(1:22,tp)

VT1wide = select(VT1wide, Pz, CPz, Cz, P4, P3, CP4, CP3, C3, FCz, C4, Time, Subject, Condition)
VT1wide$Condition = factor(VT1wide$Condition)

dat = VT1wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

VT1long = gather(dat[,1:12], "Electrode", "meanAmp_factor", 3:12)
# add condition information
VT1long$Race = NA
VT1long$Race[grep("B", VT1long$Condition)] = "Black"
VT1long$Race[grep("W", VT1long$Condition)] = "White"

VT1long$Fix = NA
VT1long$Fix[grep("E", VT1long$Condition)] = "eyes"
VT1long$Fix[grep("F", VT1long$Condition)] = "fore"

write.table(VT1long, "./PCA/1 Eval task/A- Temporo-spatial PCA (noEOG)/3 Data for MLM/VT1_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-2 --------------------------------------------------------------------

# read in data for temporal component TF11_SF1
VT2dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF11SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT2dat = rbind(VT2dat, temp)
  }
}
VT2dat$Subject = as.integer(VT2dat$Subject)


# select electrodes of interest, time of interest (115-180ms)
VT2wide = VT2dat[VT2dat$Time >= 115 & VT2dat$Time <= 180,]
tp = VT2wide[,1:22] %>% # figure out what electrodes to use
  summarise_each(funs(min)) %>% 
  sort()
plot(1:22,tp)

VT2wide = select(VT2wide, FCz, Cz, CPz, Fz, FC4, FC3, C4, C3, Time, Subject, Condition)
VT2wide$Condition = factor(VT2wide$Condition)

dat = VT2wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

VT2long = gather(dat[,1:10], "Electrode", "meanAmp_factor", 3:10)
# add condition information
VT2long$Race = NA
VT2long$Race[grep("B", VT2long$Condition)] = "Black"
VT2long$Race[grep("W", VT2long$Condition)] = "White"

VT2long$Fix = NA
VT2long$Fix[grep("E", VT2long$Condition)] = "eyes"
VT2long$Fix[grep("F", VT2long$Condition)] = "fore"

write.table(VT2long, "./PCA/1 Eval task/A- Temporo-spatial PCA (noEOG)/3 Data for MLM/VT2_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-3 --------------------------------------------------------------------

# read in data for temporal component TF08_SF1
VT3dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF08SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT3dat = rbind(VT3dat, temp)
  }
}
VT3dat$Subject = as.integer(VT3dat$Subject)

# select electrodes of interest, time of interest (145-230ms)
VT3wide = VT3dat[VT3dat$Time >= 145 & VT3dat$Time <= 230,]
tp = VT3wide[,1:22] %>% # figure out what electrodes to use
  summarise_each(funs(min)) %>% 
  sort()
plot(1:22,tp)

VT3wide = select(VT3wide, CPz, Cz, Pz, FCz, C3, CP3, P3, CP4, FC3, C4, Time, Subject, Condition)
VT3wide$Condition = factor(VT3wide$Condition)

dat = VT3wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

VT3long = gather(dat[,1:12], "Electrode", "meanAmp_factor", 3:12)
# add condition information
VT3long$Race = NA
VT3long$Race[grep("B", VT3long$Condition)] = "Black"
VT3long$Race[grep("W", VT3long$Condition)] = "White"

VT3long$Fix = NA
VT3long$Fix[grep("E", VT3long$Condition)] = "eyes"
VT3long$Fix[grep("F", VT3long$Condition)] = "fore"

write.table(VT3long, "./PCA/1 Eval task/A- Temporo-spatial PCA (noEOG)/3 Data for MLM/VT3_longDatForMLM.txt", row.names = F, sep = "\t")



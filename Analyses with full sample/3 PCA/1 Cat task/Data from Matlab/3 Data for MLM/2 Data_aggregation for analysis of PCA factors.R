require(magrittr)
require(dplyr)
require(tidyr)

path = "./PCA/1 Cat task/Data from Matlab/2 Factor waveforms separated by subject/CatTempSpatial_noEOG_3Sfactors_"

# for electrode names
ced = read.delim("./PCA/Bartholow_CED_v3_ERP-fix.txt", header = T) # ced used in EP toolkit (excludes EOG and nose but includes mastoid references)
factorelec = as.character(ced$labels)

condList = c("BE", "BF", "WE", "WF")
subList = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:64, 66)


# VT-1 --------------------------------------------------------------------

# read in data for temporal component TF09_SF1
VT1dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF09SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT1dat = rbind(VT1dat, temp)
  }
}
VT1dat$Subject = as.integer(VT1dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT1dat, aes(Time, Pz, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis


# select electrodes of interest, time of interest (80-140ms)
VT1wide = select(VT1dat, Pz, CPz, Cz, P3, P4, CP3, CP4, Cz, C3, Time, Subject, Condition)
VT1wide = VT1wide[VT1wide$Time >= 80 & VT1wide$Time <= 140,]
VT1wide$Condition = factor(VT1wide$Condition)

dat = VT1wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

VT1long = gather(dat[,1:10], "Electrode", "meanAmp_factor", 3:10)
# add condition information
VT1long$Race = NA
VT1long$Race[grep("B", VT1long$Condition)] = "Black"
VT1long$Race[grep("W", VT1long$Condition)] = "White"

VT1long$Fix = NA
VT1long$Fix[grep("E", VT1long$Condition)] = "eyes"
VT1long$Fix[grep("F", VT1long$Condition)] = "fore"

write.table(VT1long, "./PCA/A- Temporo-spatial (no EOG)/Data for MLM/VT1_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-2 --------------------------------------------------------------------

# read in data for temporal component TF07_SF1
VT2dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF07SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT2dat = rbind(VT2dat, temp)
  }
}
VT2dat$Subject = as.integer(VT2dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT2dat[VT2dat$Subject == 2,], aes(Time, FCz, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis

# select electrodes of interest, time of interest (115-180ms)
VT2wide = select(VT2dat, FCz, Cz, Fz, CPz, FC4, FC3, Time, Subject, Condition)
VT2wide = VT2wide[VT2wide$Time >= 115 & VT2wide$Time <= 180,]
VT2wide$Condition = factor(VT2wide$Condition)

dat = VT2wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

VT2long = gather(dat[,1:8], "Electrode", "meanAmp_factor", 3:8)
# add condition information
VT2long$Race = NA
VT2long$Race[grep("B", VT2long$Condition)] = "Black"
VT2long$Race[grep("W", VT2long$Condition)] = "White"

VT2long$Fix = NA
VT2long$Fix[grep("E", VT2long$Condition)] = "eyes"
VT2long$Fix[grep("F", VT2long$Condition)] = "fore"

write.table(VT2long, "./PCA/A- Temporo-spatial (no EOG)/Data for MLM/VT2_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-3 --------------------------------------------------------------------

# read in data for temporal component TF06_SF1
VT3dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste(path, i, "_", j, "_TF06SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = -100:1000
    temp$Subject = i
    temp$Condition = j
    VT3dat = rbind(VT3dat, temp)
  }
}
VT3dat$Subject = as.integer(VT3dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT3dat[VT3dat$Subject == 1,], aes(Time, Cz, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis

# select electrodes of interest, time of interest (145-230ms)
VT3wide = select(VT3dat, Cz, FCz, CPz, C3, FC3, Fz, Pz, CP3, FC4, C4, Time, Subject, Condition)
VT3wide = VT3wide[VT3wide$Time >= 145 & VT3wide$Time <= 230,]
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

write.table(VT3long, "./PCA/A- Temporo-spatial (no EOG)/Data for MLM/VT3_longDatForMLM.txt", row.names = F, sep = "\t")

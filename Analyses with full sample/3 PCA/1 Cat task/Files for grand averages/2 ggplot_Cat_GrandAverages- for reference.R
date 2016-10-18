# Grand average consolidator & plotter
# Adapted from script written by HARD KREW on 02/20/2014 (Updated & generalized for lab on 03/13/2014)
require(dplyr)

# data seem to be tab-delimited...
# messed up headers so we want to make our own

# column names are a mess b/c of stupid bracketing convention from SCAN
# So I make a .txt file of "pretty" column names manually
# just make sure they're in the right order or else you're screwed!
electrodeNamesP2=read.delim("./Analyses with full sample/ERP quant data/ggplot figures/electrodeNames_P2.txt", header = F)
electrodeNamesP2 = as.character(electrodeNamesP2$V1) # converts to vector of strings

# loop .dat reading
# Have to do it separately for AvgMast ref and Nose ref
# taskFolderList = c("1 Eval dat/", "2 Cat dat/") # Study 2
race = c("Black", "White")
fix = c("eyes", "fore")

catP2 = NULL
for (l in race) {
  for (m in fix) {
    fileName = paste("./Analyses with full sample/ERP quant data/ggplot figures/Files exported from scan/Cat- avg mast/", l, "_", m, "_66export.dat", sep="") # generate file name
    temp = read.delim(file=fileName, skip=3, header=FALSE, colClasses = "numeric") # read in the file, skipping some header
    names(temp) = electrodeNamesP2 # replace column names
    temp = temp[,1:24] # get rid of last NA column
    # add identifiers
    temp$Points = 1:1101
    temp$Time = -100:1000
    temp$Race = l
    temp$Fix = m
    temp$Reference = "avg mast"
    temp$Condition = paste(l, m, sep = "_")
    # bind temp data to full data set
    catP2 = rbind(catP2, temp)
  }
}

catP2$Race = as.factor(catP2$Race)
catP2$Fix = as.factor(catP2$Fix)
catP2$Condition = as.factor(catP2$Condition)


# do the same for nose referenced data (doesn't include OZ)
electrodeNamesN170=read.delim("./Analyses with full sample/ERP quant data/ggplot figures/electrodeNames_N170.txt", header = F)
electrodeNamesN170 = as.character(electrodeNamesN170$V1) # converts to vector of strings

catN170 = NULL
for (l in race) {
  for (m in fix) {
    fileName = paste("./Analyses with full sample/ERP quant data/ggplot figures/Files exported from scan/Cat- nose/", l, "_", m, "_65export.dat", sep="") # generate file name
    temp = read.delim(file=fileName, skip=3, header=FALSE, colClasses = "numeric") # read in the file, skipping some header
    names(temp) = electrodeNamesN170 # replace column names
    temp = temp[,1:23] # get rid of last NA column
    # add identifiers
    temp$Points = 1:1101
    temp$Time = -100:1000
    temp$Race = l
    temp$Fix = m
    temp$Reference = "nose"
    temp$Condition = paste(l, m, sep = "_")
    # bind temp data to full data set
    catN170 = rbind(catN170, temp)
  }
}

catN170$Race = as.factor(catN170$Race)
catN170$Fix = as.factor(catN170$Fix)
catN170$Condition = as.factor(catN170$Condition)




#####################################################
require(ggplot2)
require(colorspace)
require(grid)

# Set basic aspects of plots ----------------------------------------------

P2box = annotate("rect",    #P2
                 xmin=130, xmax=190, ymin=-Inf, ymax=Inf, 
                 alpha=.45,
                 fill="#F0E0FF",
                 color="red") 
N170box =  annotate("rect",    #N170
                    xmin=130, xmax=215, ymin=-Inf, ymax=Inf,
                    alpha=0.45,
                    fill="#F0E0FF",
                    color="green")

ERPline = geom_line(lwd=1.1,
                    linetype="solid",
                    aes(color = Condition))

condColors <- c("Black_eyes" = "red3", 
                "Black_fore" = "darkorange", 
                "White_eyes" = "dodgerblue4", 
                "White_fore" = "dodgerblue2")

none = element_blank() 


# P2 plots- individual electrodes ----------------------------------------------------------------

plot.P2.Cat.CZ =
  ggplot(data=catP2, aes(Time, CZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CZ", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_CZ.png", plot.P2.Cat.CZ, width=8, height=4, units="in")


plot.P2.Cat.CPZ =
  # set default parameters in ggplot()
  ggplot(data=catP2, aes(Time, CPZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CPZ", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_CPZ.png", plot.P2.Cat.CPZ, width=8, height=4, units="in")


plot.P2.Cat.PZ =
  # set default parameters in ggplot()
  ggplot(data=catP2, aes(Time, PZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "PZ", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_PZ.png", plot.P2.Cat.PZ, width=8, height=4, units="in")


plot.P2.Cat.C3 =
  ggplot(data=catP2, aes(Time, C3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "C3", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_C3.png", plot.P2.Cat.C3, width=8, height=4, units="in")

plot.P2.Cat.C4 =
  ggplot(data=catP2, aes(Time, C4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "C4", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_C4.png", plot.P2.Cat.C4, width=8, height=4, units="in")

plot.P2.Cat.CP3 =
  ggplot(data=catP2, aes(Time, CP3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CP3", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_CP3.png", plot.P2.Cat.CP3, width=8, height=4, units="in")

plot.P2.Cat.CP4 =
  ggplot(data=catP2, aes(Time, CP4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CP4", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_CP4.png", plot.P2.Cat.CP4, width=8, height=4, units="in")

plot.P2.Cat.P3 =
  ggplot(data=catP2, aes(Time, P3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "P3", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_P3.png", plot.P2.Cat.P3, width=8, height=4, units="in")

plot.P2.Cat.P4 =
  ggplot(data=catP2, aes(Time, P4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "P4", x = -30, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_P4.png", plot.P2.Cat.P4, width=8, height=4, units="in")


# P2 plots- aggregated electrodes -----------------------------------------

# compute average of several electrodes
aggrList1 = c("CZ", "C3", "C4", "CP3", "CP4", "CPZ", "PZ")
catP2$avgElecP2 = apply(catP2[,colnames(catP2) %in% aggrList1], 1, FUN=mean)

plot.P2.Cat.avgElec =
  ggplot(data=catP2, aes(Time, avgElecP2, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "Average of", x = -75, y = -7, size = 6, colour = "black", hjust = 0) +
  annotate("text", label = "CZ, C3, C4, CPZ", x = -75, y = -5.5, size = 6, colour = "black", hjust = 0) +
  annotate("text", label = "CP3, CP4, PZ", x = -75, y = -4, size = 6, colour = "black", hjust = 0) +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 800), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Cat_avgElec.png", plot.P2.Cat.avgElec, width=8, height=4, units="in")




# N170 plots- individual electrodes ------------------------------------------------------------

plot.N170.Cat.TP7 =
  ggplot(data=catN170, aes(Time, TP7, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "TP7", x = -50, y = -5.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(8, -7)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Cat_TP7.png", plot.N170.Cat.TP7, width=7, height=4, units="in")


plot.N170.Cat.TP8 =
  ggplot(data=catN170, aes(Time, TP8, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "TP8", x = -50, y = -5.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(8, -7)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Cat_TP8.png", plot.N170.Cat.TP8, width=6, height=4, units="in")


plot.N170.Cat.P7 =
  ggplot(data=catN170, aes(Time, P7, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "P7", x = -50, y = -5.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(8, -7)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Cat_P7.png", plot.N170.Cat.P7, width=6, height=4, units="in")

plot.N170.Cat.P8 =
  ggplot(data=catN170, aes(Time, P8, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "P8", x = -50, y = -5.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(8, -7)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Cat_P8.png", plot.N170.Cat.P8, width=6, height=4, units="in")


# N170 plots- aggregated electrodes ---------------------------------------

# # list your electrone names (CASE-SENSITIVE) that you wish to aggregate over
aggrList1 = c("TP7", "TP8", "P7", "P8")
catN170$avgElec= apply(catN170[,colnames(catN170) %in% aggrList1], 1, FUN=mean)

plot.N170.Cat.avgElec =
  ggplot(data=catN170, aes(Time, avgElec, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "Average of", x = -75, y = -5.8, size = 6, colour = "black", hjust = 0) +
  annotate("text", label = "P8, P7, TP8, TP7", x = -75, y = -4.5, size = 6, colour = "black", hjust = 0) +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(8, -7)) +  # flips y axis
  scale_color_manual(values=condColors)

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Cat_avgElec.png", plot.N170.Cat.avgElec, width=6, height=4, units="in")







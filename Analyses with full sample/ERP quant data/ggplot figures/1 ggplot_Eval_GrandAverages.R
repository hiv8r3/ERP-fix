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

evalP2 = NULL
for (l in race) {
  for (m in fix) {
      fileName = paste("./Analyses with full sample/ERP quant data/ggplot figures/Files exported from scan/Eval- avg mast/", l, "_", m, "_66export.dat", sep="") # generate file name
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
      evalP2 = rbind(evalP2, temp)
  }
}

evalP2$Race = as.factor(evalP2$Race)
evalP2$Fix = as.factor(evalP2$Fix)
evalP2$Condition = as.factor(evalP2$Condition)


# do the same for nose referenced data (doesn't include OZ)
electrodeNamesN170=read.delim("./Analyses with full sample/ERP quant data/ggplot figures/electrodeNames_N170.txt", header = F)
electrodeNamesN170 = as.character(electrodeNamesN170$V1) # converts to vector of strings

evalN170 = NULL
for (l in race) {
  for (m in fix) {
    fileName = paste("./Analyses with full sample/ERP quant data/ggplot figures/Files exported from scan/Eval- nose/", l, "_", m, "_66export.dat", sep="") # generate file name
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
    evalN170 = rbind(evalN170, temp)
  }
}

evalN170$Race = as.factor(evalN170$Race)
evalN170$Fix = as.factor(evalN170$Fix)
evalN170$Condition = as.factor(evalN170$Condition)




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

plot.P2.Eval.CZ =
  ggplot(data=evalP2, aes(Time, CZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CZ", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                   limits=c(-100, 410), 
                   expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                   breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_CZ.png", plot.P2.Eval.CZ, width=6, height=4, units="in")


plot.P2.Eval.CPZ =
  # set default parameters in ggplot()
  ggplot(data=evalP2, aes(Time, CPZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CPZ", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_CPZ.png", plot.P2.Eval.CPZ, width=6, height=4, units="in")


plot.P2.Eval.PZ =
  # set default parameters in ggplot()
  ggplot(data=evalP2, aes(Time, PZ, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "PZ", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_PZ.png", plot.P2.Eval.PZ, width=6, height=4, units="in")

plot.P2.Eval.C3 =
  ggplot(data=evalP2, aes(Time, C3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "C3", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_C3.png", plot.P2.Eval.C3, width=6, height=4, units="in")

plot.P2.Eval.C4 =
  ggplot(data=evalP2, aes(Time, C4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "C4", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_C4.png", plot.P2.Eval.C4, width=6, height=4, units="in")

plot.P2.Eval.CP3 =
  ggplot(data=evalP2, aes(Time, CP3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CP3", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_CP3.png", plot.P2.Eval.CP3, width=6, height=4, units="in")

plot.P2.Eval.CP4 =
  ggplot(data=evalP2, aes(Time, CP4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "CP4", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_CP4.png", plot.P2.Eval.CP4, width=6, height=4, units="in")

plot.P2.Eval.P3 =
  ggplot(data=evalP2, aes(Time, P3, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "P3", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_P3.png", plot.P2.Eval.P3, width=6, height=4, units="in")

plot.P2.Eval.P4 =
  ggplot(data=evalP2, aes(Time, P4, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "P4", x = -50, y = -6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_P4.png", plot.P2.Eval.P4, width=6, height=4, units="in")


# P2 plots- aggregated electrodes -----------------------------------------

# compute average of several electrodes
aggrList1 = c("CZ", "C3", "C4", "CP3", "CP4", "CPZ", "PZ")
evalP2$avgElecP2 = apply(evalP2[,colnames(evalP2) %in% aggrList1], 1, FUN=mean)

plot.P2.Eval.avgElec =
  ggplot(data=evalP2, aes(Time, avgElecP2, group = Condition)) + 
  ERPline + 
  P2box + 
  # add label for electrode
  annotate("text", label = "Average of", x = -75, y = -7, size = 6, colour = "black", hjust = 0) +
  annotate("text", label = "CZ, C3, C4, CPZ", x = -75, y = -5.5, size = 6, colour = "black", hjust = 0) +
  annotate("text", label = "CP3, CP4, PZ", x = -75, y = -4, size = 6, colour = "black", hjust = 0) +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(7.5, -7.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/P2_Eval_avgElec.png", plot.P2.Eval.avgElec, width=6, height=4, units="in")




# N170 plots- individual electrodes ------------------------------------------------------------

plot.N170.Eval.TP7 =
  ggplot(data=evalN170, aes(Time, TP7, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "TP7", x = -50, y = -5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(5.5, -5.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Eval_TP7.png", plot.N170.Eval.TP7, width=6, height=4, units="in")

plot.N170.Eval.TP8 =
  ggplot(data=evalN170, aes(Time, TP8, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "TP8", x = -50, y = -5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(5.5, -5.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Eval_TP8.png", plot.N170.Eval.TP8, width=6, height=4, units="in")


plot.N170.Eval.P7 =
  ggplot(data=evalN170, aes(Time, P7, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "P7", x = -50, y = -5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(5.5, -5.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Eval_P7.png", plot.N170.Eval.P7, width=6, height=4, units="in")

plot.N170.Eval.P8 =
  ggplot(data=evalN170, aes(Time, P8, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "P8", x = -50, y = -5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(5.5, -5.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Eval_P8.png", plot.N170.Eval.P8, width=6, height=4, units="in")


# N170 plots- aggregated electrodes ---------------------------------------

# # list your electrone names (CASE-SENSITIVE) that you wish to aggregate over
aggrList1 = c("TP7", "TP8", "P7", "P8")
evalN170$avgElec= apply(evalN170[,colnames(evalN170) %in% aggrList1], 1, FUN=mean)

plot.N170.Eval.avgElec =
  ggplot(data=evalN170, aes(Time, avgElec, group = Condition)) + 
  ERPline + 
  N170box + 
  # add label for electrode
  annotate("text", label = "Average of", x = -75, y = -5.3, size = 8, colour = "black", hjust = 0) +
  annotate("text", label = "P8, P7, TP8, TP7", x = -75, y = -4.0, size = 8, colour = "black", hjust = 0) +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 410), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_reverse("Amplitude (uV)", limits =c(5.5, -5.5)) +  # flips y axis
  scale_color_manual(values=condColors) +
  guides(color=FALSE) # Remove legend 

ggsave(filename="./Analyses with full sample/ERP quant data/ggplot figures/Figures/N170_Eval_avgElec.png", plot.N170.Eval.avgElec, width=6, height=4, units="in")













# Extra Meredith stuff ----------------------------------------------------


  #labs(title="Between-Subjects Comparison of Attend, Suppress, and Reappraise (Block 1) \n(Composite of 3x3 Central/Frontal Electrodes)")
  #geom_text(x=60, y=4, aes(label=label), data=ERN_label) + 
  # geom_text(x=200, y=4, aes(label=label), data=Pe_label) +
  # theme(strip.text = element_text(size=rel(1)),
  #       strip.background = element_rect(fill="grey90")) +
  # geom_text(x=0, y=4.3, aes(label=label), data=Resp_label, 
  #           color="orange") +
  # geom_segment(aes(x=0, xend=0, y=4.5, yend=5), 
  #              color="orange", size=1, arrow=arrow(length=unit(.2,"cm"))) +




#################################################
# Pe #
## Block 1:
#composite avg of 3x3 mid electrodes:
plot.pe = 
  ggplot(data=dat.b1, 
         aes(x=Time, y=avg3x3Mid)) + 
  scale_x_continuous("Time (ms)",
                     limits=c(-100, 400), 
                     expand=c(0,0),
                     breaks=c(-100, 0, 100, 200, 300)) +
  annotate("rect",    #Pe
           xmin=150, xmax=250, ymin=-Inf, ymax=Inf,
           alpha=0.35,
           fill="#FFCCCC",
           color="#FFD1D1") +
  scale_color_manual(values=WITcolors) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (?V)") +
  facet_wrap(~Instr, ncol=3) +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  geom_line(lwd=1.1, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  #labs(title="Between-Subjects Comparison of Attend, Suppress, and Reappraise (Block 1) \n(Composite of 3x3 Central/Frontal Electrodes)")
  #geom_text(x=60, y=4, aes(label=label), data=ERN_label) + 
  # geom_text(x=200, y=4, aes(label=label), data=Pe_label) +
  theme(strip.text = element_text(size=rel(1)),
        strip.background = element_rect(fill="grey90")) +
  guides(color=FALSE) # Remove legend 

plot.pe 

plot.pe.arrow <- plot.pe + geom_text(x=0, y=-5.5, aes(label=label), data=Resp_label, 
                                     color="orange", size=3) +
  geom_segment(aes(x=0, xend=0, y=5.8, yend=6.3), 
               color="orange", size=1, arrow=arrow(length=unit(.2,"cm"))) 
plot.pe.arrow

ggsave(filename="Block1_Pe_150-250ms_3x3centered@Cz.png", plot.pe, width=10, height=4, units="in")

ggsave(filename="Block1_Pe_150-250ms_3x3centered@Cz_RespArrow.png", plot.pe.arrow, width=10, height=4, units="in")
#ggsave(filename="Block1_FCz.wmf", wrapPlot.B1.FCz, width=9, height=3.25, units="in")













# store a ggplot to an object
plot.Sup.B1.FCz =
  # set default parameters in ggplot()
  ggplot(data=dat[dat$Instr=="Suppress",], aes(x=Time, y=FCZ, col=Cond)) + 
  # add continuous x-axis with scale, limits, and breaks at relevant times
  # expand=c(0,0) removes extra space before & after data
  scale_x_continuous("Time (ms)",limits=c(-200, 400), expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 130, 150, 200, 250, 300, 400)) +
  # draw the line connecting the data. Alter LWD to change line width.
  # In aes(lty=) you can set a factor to map onto line type (dash, dot, etc)
  geom_line(lwd=1, linetype="solid", aes(color=Cond)) +
  #geom_line(lwd=1, aes(linetype=cond)) +
  #Changing colors to the ones set above (see WITcolors):
  scale_color_manual(values=WITcolors) +
  # adding theme_bw() removes the gray grid from the plot background
  theme_bw() + 
  # draw the y=0 microvolt axis for reference
  geom_hline(y=0) +
  # invert the y-axis (negative up) & label it
  scale_y_reverse("Amplitude (mV)") +
  # Put lines in greyscale. If doing color, remove this line. If printing B&W, remove.
  #scale_colour_grey() +
  # more stuff removing background gridlines
  theme(panel.grid.major = none, panel.grid.minor = none) 





############color testing

#WITcolors <- c("bt" = "#0571B0", "bg" = "#4DAC26", "wt" = "#92C5DE", "wg" = "#B8E186")
#WITcolors <- c(c("bt" = "#3366FF", "bg" = "#4DAC26", "wt" = "#33CCFF", "wg" = "#B8E186"))
#WITcolors <- c(c("bt" = "#3366FF", "bg" = "#4DAC26", "wt" = "#33CCFF", "wg" = "#B8E186"))

#Final choices (i think...)
WITcolors <- c("bt" = "#0099FF", "bg" = "#29A329", "wt" = "#80CCFF", "wg" = "#85E085")

plot.testing.new + theme(legend.position=c(1,1), 
                         legend.justification=c(1,1))

## Remove legend: 
plot.testing.new + guides(color=FALSE)

#####ADding mu to x axis label - can't figure it out...
plot.testing.new + scale_y_reverse(expression(mu))
plot.testing.new + scale_y_reverse(paste("Amplitude (", expression(mu), "V"))
yaxistitle <- paste("Amplitude (", m, "V")

plot.testing.new + xlab()

subblock <- paste(active$sub, Instr, sep="")

m <- expression(mu)


#Facet wrap:

#### FCZ: Block 1
wrapPlot.B1.FCz =
  # set default parameters in ggplot()
  ggplot(data=dat.B1, 
         aes(x=Time, y=FCZ)) + 
  # expand=c(0,0) removes extra space before & after data
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 200, 300)) +
  # draw the line connecting the data. Alter LWD to change line width.
  # In aes(lty=) you can set a factor to map onto line type (dash, dot, etc)
  geom_line(lwd=1, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  #Changing colors to the ones set above (see WITcolors):
  scale_color_manual(values=WITcolors) +
  #                   limits=c("Black+Tool", "Black+Gun", "White+Tool", "White+Gun"),
  #                   name = "WIT Conditions") +
  # adding theme_bw() removes the gray grid from the plot background
  theme_bw() + 
  # draw the y=0 microvolt axis for reference
  geom_hline(y=0) +
  # invert the y-axis (negative up) & label it
  scale_y_reverse("Amplitude (mV)") +
  # Put lines in greyscale. If doing color, remove this line. If printing B&W, remove.
  #scale_colour_grey() +
  # more stuff removing background gridlines
  # specify a formula in facet_wrap and it will break the plots up accordingly
  # this is a great way to plot interactions.
  # a formula is e.g. ~Condition or even ~Condition * Valence
  facet_wrap(~Instr, ncol=3) +
  #facet_wrap(~Instr, ncol=3, scales="free_y") +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  labs(title="Between-Subjects Comparison of Attend, Suppress, and Reappraise (Block 1) \n(Electrode: FCz)")

wrapPlot.B1.FCz 

#ggsave(filename="Block1_FCz.png", wrapPlot.B1.FCz, width=9, height=3.25, units="in")
#ggsave(filename="Block1_FCz.wmf", wrapPlot.B1.FCz, width=9, height=3.25, units="in")


#CZ:
wrapPlot.B1.Cz =
  ggplot(data=dat.B1, 
         aes(x=Time, y=CZ)) + 
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 200, 300)) +
  geom_line(lwd=1, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  scale_color_manual(values=WITcolors) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (mV)") +
  facet_wrap(~Instr, ncol=3) +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  labs(title="Attend, Suppress, and Reappraise as Block 1 (Electrode: Cz)")
wrapPlot.B1.Cz

#SAVE FILE
#ggsave(filename="Block1_Cz.png", wrapPlot.B1.Cz, width=9, height=3.25, units="in")
#ggsave(filename="Block1_Cz.wmf", wrapPlot.B1.Cz, width=9, height=3.25, units="in")

shading.ern <- annotate("rect", 
                        xmin=30, xmax=110, ymin=-Inf, ymax=Inf,
                        alpha=0.35,
                        fill="#F0E0FF",
                        color="#F0E0FF") 


##############################################
#Facet wrap with composite avg of FCZ and CZ:
wrapPlot.B1.2electr =
  # set default parameters in ggplot()
  ggplot(data=dat.B1, 
         aes(x=Time, y=avgFczCz)) + 
  # expand=c(0,0) removes extra space before & after data
  shading.ern +
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 200, 300)) +
  geom_line(lwd=1, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  scale_color_manual(values=WITcolors) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (mV)") +
  facet_wrap(~Instr, ncol=3) +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  #+ labs(title="Attend, Suppress, and Reappraise as Block 1 (Electrode: Composite of FCz and Cz)")
  guides(color=FALSE) # Remove legend 

wrapPlot.B1.2electr 


##### Save files ####
ggsave(filename="Block1Panel_FCzCzAvg_ERN highlighted_no legend.wmf", wrapPlot.B1.2electr, width=10, height=4.5, units="in")
ggsave(filename="Block1_FCzCzAvg_ERN30-110 highlighted_no legend.png", wrapPlot.B1.2electr, width=10, height=4.5, units="in")


#Facet wrap with composite avg of 3x3 electrodes:
wrapPlot.B1.3x3 =
  ggplot(data=dat.B1, 
         aes(x=Time, y=avg3x3MidFront)) + 
  # expand=c(0,0) removes extra space before & after data
  shading.ern +
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 200, 300)) +
  geom_line(lwd=1, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  scale_color_manual(values=WITcolors) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (mV)") +
  facet_wrap(~Instr, ncol=3) +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  labs(title="Attend, Suppress, and Reappraise as Block 1 (Electrode: Composite of 3 x 3)") +
  guides(color=FALSE) # Remove legend 

wrapPlot.B1.3x3 

##### Save files ####
#ggsave(filename="Block1Panel_3x3Avg_ERN highlighted_no legend.wmf", wrapPlot.B1.2electr, width=10, height=4.5, units="in")
ggsave(filename="Block1_3x3Avg_ERN30-110 highlighted_no legend.png", wrapPlot.B1.3x3, width=10, height=4.5, units="in")






####For SPR Poster!!!
#####################

ERN_label <-data.frame(Instr=c("Attend"), label = c("ERN"))
Pe_label <-data.frame(Instr=c("Attend"), label = c("Pe"))
WITcolors <- c("bt" = "#0099FF", 
               "bg" = "#33A329", 
               "wt" = "#80CCFF", 
               "wg" = "#8AE62E")
Resp_label <- data.frame(Instr=c("Attend"), label = c("Response"))

#Facet wrap with composite avg of 3x3 mid/front electrodes:
wrapPlot.B1.3x3midfront =
  # set default parameters in ggplot()
  ggplot(data=dat.B1, 
         aes(x=Time, y=avg3x3MidFront)) + 
  # expand=c(0,0) removes extra space before & after data
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0,0),
                     breaks=c(-200, -100, 0, 100, 200, 300)) +
  annotate("rect",    #Pe
           xmin=150, xmax=250, ymin=-Inf, ymax=Inf,
           alpha=0.35,
           fill="#FFCCCC",
           color="#FFD1D1") +
  annotate("rect",    #ERN
           xmin=10, xmax=110, ymin=-Inf, ymax=Inf,
           alpha=0.45,
           fill="#F0E0FF",
           color="#F0E0FF") +
  geom_line(lwd=1.2, 
            linetype="solid", 
            aes(linetype=Instr, color=Cond)) +
  scale_color_manual(values=WITcolors) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (mV)") +
  facet_wrap(~Instr, ncol=3) +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  #labs(title="Between-Subjects Comparison of Attend, Suppress, and Reappraise (Block 1) \n(Composite of 3x3 Central/Frontal Electrodes)")
  geom_text(x=60, y=4, aes(label=label), data=ERN_label) + 
  geom_text(x=200, y=4, aes(label=label), data=Pe_label) +
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="grey90")) +
  geom_text(x=0, y=-4.7, aes(label=label), data=Resp_label, 
            color="orange") +
  annotate("segment", x=0, xend=0, y=5.1, yend=-6, 
           color="orange", size=1,
           arrow=arrow(length=unit(.3,"cm"))) +
  #shading.pe + shading.ern +
  #+ labs(title="Attend, Suppress, and Reappraise as Block 1 (Electrode: Composite of FCz and Cz)")
  guides(color=FALSE) # Remove legend 

wrapPlot.B1.3x3midfront 




+ geom_text(x=0, y=-4.7, aes(label=label), data=Resp_label, 
            color="orange") +
  annotate("segment", x=0, xend=0, y=5.1, yend=-6, 
           color="orange", 
           arrow=arrow(length=unit(.3,"cm")))

#must load grid package for arrows


#Testing:
wrapPlot.B1.3x3midfront + scale_color_manual(values=c("bt" = "#0099FF", 
                                                      "bg" = "#33A329", 
                                                      "wt" = "#80CCFF", 
                                                      "wg" = "#8AE62E"))

"bg" = "#29A329" #009900 #19A319, "wg" = "#85E085"75FF47#66C266#7ACC29


+ theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="grey90"))
#Annotate with labels for electrodes (on first facet only): 
ERN_label <-data.frame(Instr=c("Attend"), label = c("ERN"))
Pe_label <-data.frame(Instr=c("Attend"), label = c("Pe"))
wrapPlot.B1.3x3midfront + geom_text(x=60, y=4, aes(label=label), data=ERN_label) + 
  geom_text(x=200, y=4, aes(label=label), data=Pe_label)

shading.pe <- annotate("rect", 
                       xmin=150, xmax=250, ymin=-Inf, ymax=Inf,
                       alpha=0.2,
                       fill="#FFCCCC",
                       color="#FFE6E6") 

shading.ern <- annotate("rect", 
                        xmin=10, xmax=110, ymin=-Inf, ymax=Inf,
                        alpha=0.3,
                        fill="#F0E0FF",
                        color="#F0E0FF") 

ggsave(filename="Block1_3x3Avg_ERN and PE highlighted_no legend_annotated_Poster2.png", wrapPlot.B1.3x3midfront, width=9, height=3.75, units="in")

ggsave(filename="Block1_3x3Avg_ERN and PE highlighted_no legend.wmf", wrapPlot.B1.3x3midfront, width=9, height=3.5, units="in")


###################################################
###################################################
### LONG DAT! #####################################
###################################################

dat.prelong =dat.B1[,c(-11, -23, -26)]
dat.prelong.shortlist = dat.prelong[, c(-10, -11, -12, -13, -16, -19, -24, -25, -26, -27, -28)]

#Reshape the data into long format. 
longdat <-reshape(dat.prelong.shortlist,
                  varying = c("FZ", "FCZ", "CZ", "F3", "F4", "FC3", "FC4", "C3", "C4", "CP3", "CPZ", "CP4"),
                  v.names = "mean",
                  timevar = "Channel", 
                  times = c("FZ", "FCZ", "CZ", "F3", "F4", "FC3", "FC4", "C3", "C4", "CP3", "CPZ", "CP4"), 
                  new.row.names = 1:4000000,
                  direction = "long")

longdat$coronal[longdat$Channel %in% c("F3", "FZ", "F4")]="1_F"  #if "channel" is F-something, coronal=0
longdat$coronal[longdat$Channel %in% c("FC3", "FCZ", "FC4")]="2_FC"	#if "channel" is FC-something, coronal=1
longdat$coronal[longdat$Channel %in% c("C3", "CZ", "C4")]="3_C"	
longdat$coronal[longdat$Channel %in% c("CP3", "CPZ", "CP4")]="4_CP"
longdat$coronal[longdat$Channel %in% c("P3", "PZ", "P4")]="5_P"
#longdat$coronal[longdat$Channel %in% c("T5.P7", "P3", "PZ", "P4", "T6.P8")]="5_P"
#longdat$coronal[longdat$Channel %in% c("PO8", "PO7")]="6PO"
#longdat$sagittal=1:dim(longdat)[1]						#again, placeholders
longdat$sagittal[longdat$Channel %in% c("F3", "FC3", "C3", "CP3", "P3")]="-1_3Left" #"-2Left"
longdat$sagittal[longdat$Channel %in% c("FZ", "FCZ", "CZ", "CPZ", "PZ")]="0_z" #"0Midline"
longdat$sagittal[longdat$Channel %in% c("F4", "FC4", "C4", "CP4", "P4")]="1_4Right" #"2Right"
#longdat$sagittal[longdat$Channel %in% c("PO7", "T5.P7")]="TPLeft" #"-4Left"
#longdat$sagittal[longdat$Channel %in% c("PO8", "T6.P8")]="TPRight" #"4Right"


plot.B1.3x4 =
  ggplot(data=longdat[longdat$Instr=="Attend",], 
         aes(x=Time, y=mean)) + 
  #ggplot(data=dat[dat$Response=="e",], aes(x=Time, y=X.FCZ, col=Cond))
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 400), 
                     expand=c(0, 0),
                     breaks=c(-100, 0, 100, 200, 300)) +
  shading.ern +
  geom_line(lwd=1, 
            aes(color=Cond)) +
  scale_color_manual(values=WITcolors) +
  #                   limits=c("Black+Tool", "Black+Gun", "White+Tool", "White+Gun"),
  #                   name = "WIT Conditions") +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (mV)") +
  # a formula is e.g. ~Condition or even ~Condition * Valence
  facet_wrap(~coronal * sagittal, ncol=3) +
  #facet_wrap(~Instr, ncol=3, scales="free_y") +
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  #labs(title="Attend: LPP \nBetween-Subjects Comparison (Block 1)") +
  guides(color=FALSE) # Remove legend 

plot.B1.3x4 










########################################################
### for perfection: 
# Fix font sizes of axes - see font.axis
# Fix Condition labels in legend
# Shade areas of interest along X-Axis (like 0 to 130, and 150 to 250ms)
#geom_ is perhaps a promising route. 
# geom_ribbon??? or geom_polygon? Investigation to be continued later....
# Also, can I insert a symbol into the y-axis label? ("mV") 
# Can I put the electrode label in the corner of the graph? As is standard in ERP results? 

########################################################














#######################
# can combine multiple conditions
# with a facet wrap
#
none = element_blank()

wrappedPlot = 
  ggplot(data=dat, 
         aes(x=Time, y=Composite)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-200, 1000), 
                     expand=c(0,0),
                     breaks=c(-200, 0, 300, 550, 800, 1000)) +
  geom_line(lwd=.5, aes(lty=Valence)) +
  theme_bw() + 
  geom_hline(y=0) +
  scale_y_reverse("Amplitude (?V)") +
  # specify a formula in facet_wrap and it will break the plots up accordingly
  # this is a great way to plot interactions.
  # a formula is e.g. ~Condition or even ~Condition * Valence
  facet_wrap(~Paradigm, nrow=3, scales="free_y") +
  theme(panel.grid.major = none, panel.grid.minor = none)+
  scale_linetype_manual(values=c("solid", "dotted", "longdash")) +
  labs(title="Study 1")
wrappedPlot
# what are the maximum dimensions that'll fit on an MSWORD page?
# 8.5 x 11 in document w/ 1" margins in each direction
# 6.5 x 9 inches effective space
ggsave(filename="Study1_Facet_Wrap_bw_thin.wmf", study1Plot, width=6.5, height=9, units="in")
ggsave(filename="Study1_Facet_Wrap_bw_thin.png", study1Plot, width=6.5, height=9, units="in")

# you could conceivably get up to some serious stuff with facet wrapping by melting the
# dataset, assigning codes for coronal & sagittal site, and performing a facet wrap
# accordingly. sky's the limit.
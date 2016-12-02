# put data sets for three components together to test trend
# Eval task

require(lme4)
require(lmerTest)
require(dplyr)

# read in data for all three components

VT1 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT1_longDatForMLM.txt")
VT2 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT2_longDatForMLM.txt")
VT3 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT3_longDatForMLM.txt")

# add column for component

VT1$Component = "VT1"
VT2$Component = "VT2"
VT3$Component = "VT3"

# bind three datasets together

alldat = rbind(VT1, VT2, VT3)

# take out bad subjects for Cat task
evalbad = read.delim("./Analyses with full sample/Eval_badsubs.txt")

alldat = alldat[!(alldat$Subject %in% evalbad$Subject),]

# effect code Race and Fix (default is dummy coding)
alldat$Race_effect = -1 # for Black
alldat$Race_effect[alldat$Race == "White"] = 1

alldat$Fix_effect = -1 # for eyes
alldat$Fix_effect[alldat$Fix == "fore"] = 1

# make component a continuous variable
alldat$Comp_cont = 0
alldat$Comp_cont[alldat$Component == "VT2"] = 1
alldat$Comp_cont[alldat$Component == "VT3"] = 2


# test interactions -------------------------------------------------------

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race_effect*Comp_cont+Fix_effect*Comp_cont + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = alldat)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race_effect*Comp_cont+Fix_effect*Comp_cont + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = alldat, REML= F)

sink(file = "./Analyses with full sample/3 PCA/2 Eval task/5 Analyses/Model outputs/4 all_varyingInterceptElectrode_unstand_noBS_effectCoding.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()


require(lme4)
require(lmerTest)

# changed to effect coding for Race and Fix

# VT-1 --------------------------------------------------------------------

VT1 = read.delim("./Analyses with full sample/3 PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF1_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT1 = VT1[!(VT1$Subject %in% catbad$Subject),]

# effect code Race and Fix (default is dummy coding)
VT1$Race_effect = -1 # for Black
VT1$Race_effect[VT1$Race == "White"] = 1

VT1$Fix_effect = -1 # for eyes
VT1$Fix_effect[VT1$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT1)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/1.1 VF1_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (Race*Fix|Electrode), data = VT1)
summary(m2)

# sink(file = paste(path, "Model outputs/1.2 VT1_maximalModel.txt", sep=""))
# "Doesn't converge"
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT1)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT1, REML= F)

sink(file = "./Analyses with full sample/3 PCA/1 Cat task/5 Analyses/Model outputs/1 VF1_varyingInterceptElectrode_unstand_noBS_effectCoding.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()



# VT-2 --------------------------------------------------------------------

VT2 = read.delim("./Analyses with full sample/3 PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF2_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT2 = VT2[!(VT2$Subject %in% catbad$Subject),]

# effect code Race and Fix (default is dummy coding)
VT2$Race_effect = -1 # for Black
VT2$Race_effect[VT2$Race == "White"] = 1

VT2$Fix_effect = -1 # for eyes
VT2$Fix_effect[VT2$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT2)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/2.1 VF2_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (Race*Fix|Electrode), data = VT2)
summary(m2)

# sink(file = paste(path, "Model outputs/2.2 VT2_maximalModel.txt", sep=""))
# summary(m2)
# "________________________________________________________________________________________________"
# coef(m2)
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT2)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT2, REML= F)

sink(file = "./Analyses with full sample/3 PCA/1 Cat task/5 Analyses/Model outputs/2 VF2_varyingInterceptElectrode_unstand_effectCoding.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()



# VT-3 --------------------------------------------------------------------

VT3 = read.delim("./Analyses with full sample/3 PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF3_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT3 = VT3[!(VT3$Subject %in% catbad$Subject),]

# effect code Race and Fix (default is dummy coding)
VT3$Race_effect = -1 # for Black
VT3$Race_effect[VT3$Race == "White"] = 1

VT3$Fix_effect = -1 # for eyes
VT3$Fix_effect[VT3$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT3)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/3.1 VF3_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (Race*Fix|Electrode), data = VT3)
summary(m2)

# sink(file = paste(path, "Model outputs/3.2 VT3_maximalModel.txt", sep=""))
# summary(m2)
# "________________________________________________________________________________________________"
# coef(m2)
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT3)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race_effect+Fix_effect + (Race_effect+Fix_effect|Subject) + (1|Electrode), data = VT3, REML= F)

sink(file = "./Analyses with full sample/3 PCA/1 Cat task/5 Analyses/Model outputs/3 VF3_varyingInterceptElectrode_unstand_effectCoding.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()


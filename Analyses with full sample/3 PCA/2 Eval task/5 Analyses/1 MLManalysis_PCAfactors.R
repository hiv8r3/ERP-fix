require(lme4)
require(lmerTest)

# changed to effect coding for Race and Fix

# VT-1 --------------------------------------------------------------------

VT1 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT1_longDatForMLM.txt")

# take out bad subjects for eval task
evalbad = read.delim("./Analyses with full sample/Eval_badsubs.txt")

VT1 = VT1[!(VT1$Subject %in% evalbad$Subject),]

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

# sink(file = "./Analyses with full sample/3 PCA/2 Eval task/5 Analyses/Model outputs/1 VF1_varyingInterceptElectrode_unstand_noBS_effectCoding.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# calculate 95% CI intervals and put into table with other components
CI95 = data.frame(Effect = c("Race", "Fixation"), Component = "VF1", upper = NA, lower = NA)

coef = summary(m3)$coefficients
CI95$lower[CI95$Effect == "Race"] = coef[2,1] - 2*coef[2,2]
CI95$lower[CI95$Effect == "Fixation"] = coef[3,1] - 2*coef[3,2]

CI95$upper[CI95$Effect == "Race"] = coef[2,1] + 2*coef[2,2]
CI95$upper[CI95$Effect == "Fixation"] = coef[3,1] + 2*coef[3,2]


# VT-2 --------------------------------------------------------------------

VT2 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT2_longDatForMLM.txt")

# take out bad subjects for eval task
evalbad = read.delim("./Analyses with full sample/Eval_badsubs.txt")

VT2 = VT2[!(VT2$Subject %in% evalbad$Subject),]

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

# sink(file = "./Analyses with full sample/3 PCA/2 Eval task/5 Analyses/Model outputs/2 VF2_varyingInterceptElectrode_unstand_noBS_effectCoding.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# calculate 95% CI for estimates and add to previous table
CI95temp = data.frame(Effect = c("Race", "Fixation"), Component = "VF2", upper = NA, lower = NA)

coef = summary(m3)$coefficients
CI95temp$lower[CI95temp$Effect == "Race"] = coef[2,1] - 2*coef[2,2]
CI95temp$lower[CI95temp$Effect == "Fixation"] = coef[3,1] - 2*coef[3,2]

CI95temp$upper[CI95temp$Effect == "Race"] = coef[2,1] + 2*coef[2,2]
CI95temp$upper[CI95temp$Effect == "Fixation"] = coef[3,1] + 2*coef[3,2]

CI95 = rbind(CI95, CI95temp)



# VT-3 --------------------------------------------------------------------

VT3 = read.delim("./Analyses with full sample/3 PCA/2 Eval task/Data from Matlab/3 Data for MLM/VT3_longDatForMLM.txt")

# take out bad subjects for eval task
evalbad = read.delim("./Analyses with full sample/Eval_badsubs.txt")

VT3 = VT3[!(VT3$Subject %in% evalbad$Subject),]

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

# sink(file = "./Analyses with full sample/3 PCA/2 Eval task/5 Analyses/Model outputs/3 VF3_varyingInterceptElectrode_unstand_noBS_effectCoding.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# calculate 95% CI for estimates and add to previous table
CI95temp = data.frame(Effect = c("Race", "Fixation"), Component = "VF3", upper = NA, lower = NA)

coef = summary(m3)$coefficients
CI95temp$lower[CI95temp$Effect == "Race"] = coef[2,1] - 2*coef[2,2]
CI95temp$lower[CI95temp$Effect == "Fixation"] = coef[3,1] - 2*coef[3,2]

CI95temp$upper[CI95temp$Effect == "Race"] = coef[2,1] + 2*coef[2,2]
CI95temp$upper[CI95temp$Effect == "Fixation"] = coef[3,1] + 2*coef[3,2]

CI95 = rbind(CI95, CI95temp)

write.table(CI95, "./Analyses with full sample/3 PCA/2 Eval task/5 Analyses/Model outputs/4 Eval_95confInt.txt", row.names = F, sep = "\t")

require(lme4)
require(lmerTest)


# VT-1 --------------------------------------------------------------------

VT1 = read.delim("./PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF1_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT1 = VT1[!(VT1$Subject %in% catbad$Subject),]

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT1)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/1.1 VF1_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (Race*Fix|Electrode), data = VT1)
summary(m2)

# sink(file = paste(path, "Model outputs/1.2 VT1_maximalModel.txt", sep=""))
# "Doesn't converge"
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT1)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT1, REML= F)

sink(file = "./PCA/1 Cat task/5 Analyses/Model outputs/1 VF1_varyingInterceptElectrode_unstand.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()



# VT-2 --------------------------------------------------------------------

VT2 = read.delim("./PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF2_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT2 = VT2[!(VT2$Subject %in% catbad$Subject),]

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT2)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/2.1 VF2_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (Race*Fix|Electrode), data = VT2)
summary(m2)

# sink(file = paste(path, "Model outputs/2.2 VT2_maximalModel.txt", sep=""))
# summary(m2)
# "________________________________________________________________________________________________"
# coef(m2)
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT2)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT2, REML= F)

sink(file = "./PCA/1 Cat task/5 Analyses/Model outputs/2 VF2_varyingInterceptElectrode_unstand.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()

# Varying intercept model
m4 = lmer(scale(meanAmp_factor) ~ Race*Fix + (1|Subject) + (1|Electrode), data = VT2)
m4a = lmer(scale(meanAmp_factor) ~ Race*Fix + (1|Subject) + (1|Electrode), data = VT2, REML= F)

# sink(file = paste(path, "3.1 Model outputs/2.4 VF2_varyingInterceptBoth_stand.txt", sep=""))
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()



# VT-3 --------------------------------------------------------------------

VT3 = read.delim("./PCA/1 Cat task/Data from Matlab/3 Data for MLM/VF3_longDatForMLM.txt")

# take out bad subjects for Cat task
catbad = read.delim("./Analyses with full sample/Cat_badsubs.txt")

VT3 = VT3[!(VT3$Subject %in% catbad$Subject),]

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (1|Subject) + (1|Electrode), data = VT3)
summary(m1)

# sink(file = paste(path, "3.1 Model outputs/3.1 VF3_InterceptOnly_stand.txt", sep=""))
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Maximal model
m2 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (Race*Fix|Electrode), data = VT3)
summary(m2)

# sink(file = paste(path, "Model outputs/3.2 VT3_maximalModel.txt", sep=""))
# summary(m2)
# "________________________________________________________________________________________________"
# coef(m2)
# sink()

# Random effects for subject, random intercept for electrode
m3 = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT3)
summary(m3)
m3a = lmer(meanAmp_factor ~ Race*Fix + (Race*Fix|Subject) + (1|Electrode), data = VT3, REML= F)

sink(file = "./PCA/1 Cat task/5 Analyses/Model outputs/3 VF3_varyingInterceptElectrode_unstand.txt")
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
summary(m3a)$AICtab
sink()


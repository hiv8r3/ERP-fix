# read in data
binDat = read.delim("./Analyses with full sample/ERP quant data/1 Eval dat/Binned data/AllSubs_BinnedAverages_P2_peakAmp.txt")

require(lme4)
require(lmerTest)
require(magrittr)

# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(PeakAmp ~ 1 + (1|Subject) + (1|Electrode) + (1|Bin), data = binDat)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]

# Compare to examine effect of bin- significant
m2a = lmer(PeakAmp ~ 1 + (1|Subject) + (1|Electrode), data = binDat)
anova(m1,m2a)

# Compare to examine effect of electrode- significant
m2b = lmer(PeakAmp ~ 1 + (1|Subject) + (1|Bin), data = binDat)
anova(m1,m2b)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5 P2_peakAmp_InterceptOnly_BinDat.txt")
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()


# Next step: Include fixed effects

# Varying intercept only
m3 = lmer(MeanAmp ~ Race * Fix + (1|Subject) + (1|Electrode), data = binDat)
summary(m3)
m3a = lmer(MeanAmp ~ Race * Fix + (1|Subject) + (1|Electrode), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/5.1 P2_Eval_varyingInterceptModel_BinDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# Maximal model
m4 = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (Race*Fix|Electrode) + (Race*Fix|Bin), data = binDat)
summary(m4)
coef(m4)
# fit with FIML in order to get AIC
m4a = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (Race*Fix|Electrode) + (Race*Fix|Bin), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5.2 P2_peakAmp_MaximalModel_BinDat.txt")
# "Failed to converge"
# sink()

# Random effects for Subject but only intercept for electrode
m5 = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode), data = binDat)
# fit with FIML in order to get AIC
m5a = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5.3 P2_peakAmp_randomEffectSubject_BinDat.txt")
# summary(m5)
# "________________________________________________________________________________________________"
# coef(m5)
# "________________________________________________________________________________________________"
# summary(m5a)$AICtab
# sink()

# Add bin as grouping interval (allow intercept to vary)
m6 = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (1|Bin), data = binDat)
summary(m6)
# fit with FIML in order to get AIC
m6a = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (1|Bin), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5.4 P2_peakAmp_randomEffectSubject_randomInterceptBin_BinDat.txt")
# summary(m6)
# "________________________________________________________________________________________________"
# coef(m6)
# "________________________________________________________________________________________________"
# summary(m6a)$AICtab
# sink()

binBLUPs = coef(m6)$Bin
plot(binBLUPs$`(Intercept)`)

# Add trial as grouping interval (allow slopes to vary)
m6.1 = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (Race*Fix|Bin), data = binDat)
# fit with FIML in order to get AIC
m6.1a = lmer(PeakAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (Race*Fix|Bin), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5.4.1 P2_peakAmp_randomEffectSubject_randomEffectBin_BinDat.txt")
# summary(m6.1)
# "________________________________________________________________________________________________"
# coef(m6.1)
# "________________________________________________________________________________________________"
# summary(m6.1a)$AICtab
# sink()

binBLUPs = coef(m6.1)$Bin
plot(binBLUPs$`(Intercept)`, main= "Intercept", xlab = "Bin")
plot(binBLUPs$RaceWhite,main= "Race", xlab = "Bin")
plot(binBLUPs$Fixforehead,main= "Fixation", xlab = "Bin")
plot(binBLUPs$`RaceWhite:Fixforehead`,main= "Race*Fixation interaction", xlab = "Bin")

# Add trial as fixed effect (no interactions)
# m7 = lmer(MeanAmp ~ Race * Fix + Bin + (Race*Fix|Subject) + (1|Electrode), data = binDat)
# summary(m7)
# # fit with FIML in order to get AIC
# m7a = lmer(MeanAmp ~ Race * Fix + Bin + (Race*Fix|Subject) + (1|Electrode), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/5.5 P2_randomEffectSubject_fixedBin_BinDat.txt")
# summary(m7)
# "________________________________________________________________________________________________"
# coef(m7)
# "________________________________________________________________________________________________"
# summary(m7a)$AICtab
# sink()

# Add trial as fixed effect (with interactions)
m8 = lmer(PeakAmp ~ Race * Fix * Bin + (Race*Fix|Subject) + (1|Electrode), data = binDat)
# fit with FIML in order to get AIC
m8a = lmer(PeakAmp ~ Race * Fix * Bin + (Race*Fix|Subject) + (1|Electrode), data = binDat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/5.6 P2_peakAmp_randomEffectSubject_fixedBinWithInteractions_BinDat.txt")
# summary(m8)
# "________________________________________________________________________________________________"
# coef(m8)
# "________________________________________________________________________________________________"
# summary(m8a)$AICtab
# sink()

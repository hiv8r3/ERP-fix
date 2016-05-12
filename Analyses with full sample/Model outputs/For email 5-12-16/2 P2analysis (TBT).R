# read in data
dat = read.delim("./Analyses with full sample/ERP quant data/1 Eval dat/Trial by trial data/AllSubs_TBTAverages_P2.txt")

require(lme4)
require(lmerTest)
require(magrittr)

# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), data = dat)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]

# Compare to examine effect of trial- significant
m2a = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), data = dat)
anova(m1,m2a)

# Compare to examine effect of electrode- significant
m2b = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Trial), data = dat)
anova(m1,m2b)

sink(file = "./Analyses with full sample/Model outputs/4 P2_Eval_InterceptOnly_TBTdat.txt")
summary(m1)
"________________________________________________________________________________________________"
coef(m1)
sink()


# Next step: Include fixed effects

# Varying intercept only
m3 = lmer(MeanAmp ~ Race * Fix + (1|Subject) + (1|Electrode), data = dat)
summary(m3)
m3a = lmer(MeanAmp ~ Race * Fix + (1|Subject) + (1|Electrode), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.1 P2_Eval_varyingInterceptModel_TBTdat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# Maximal model
m4 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (Race*Fix|Electrode), data = dat)
summary(m4)
coef(m4)
# fit with FIML in order to get AIC
m4a = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (Race*Fix|Electrode), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.2 P2_Eval_MaximalModel_TBTdat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

# Random effects for Subject but only intercept for electrod
m5 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode), data = dat)
summary(m5)
# fit with FIML in order to get AIC
m5a = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.3 P2_randomEffectSubject_TBTdat.txt")
# summary(m5)
# "________________________________________________________________________________________________"
# coef(m5)
# "________________________________________________________________________________________________"
# summary(m5a)$AICtab
# sink()

# Add trial as grouping interval (allow intercept to vary)
m6 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (1|Trial), data = dat)
summary(m6)
# fit with FIML in order to get AIC
m6a = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (1|Trial), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.4 P2_randomEffectSubject_randomInterceptTrial_TBTdat.txt")
# summary(m6)
# "________________________________________________________________________________________________"
# coef(m6)
# "________________________________________________________________________________________________"
# summary(m6a)$AICtab
# sink()
trialBLUPs = coef(m6)$Trial
plot(trialBLUPs$`(Intercept)`)

# Add trial as grouping interval (allow slopes to vary)
m6.1 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (Race*Fix|Trial), data = dat)
# fit with FIML in order to get AIC
m6.1a = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode) + (Race*Fix|Trial), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.4.1 P2_randomEffectSubject_randomEffectTrial_TBTdat.txt")
# summary(m6.1)
# "________________________________________________________________________________________________"
# coef(m6.1)
# "________________________________________________________________________________________________"
# summary(m6.1a)$AICtab
# sink()

trialBLUPs = coef(m6.1)$Trial
plot(trialBLUPs$`(Intercept)`)
plot(trialBLUPs$RaceWhite)
plot(trialBLUPs$Fixforehead)

# Add trial as fixed effect (no interactions)
m7 = lmer(MeanAmp ~ Race * Fix + Trial + (Race*Fix|Subject) + (1|Electrode), data = dat)
summary(m7)
# fit with FIML in order to get AIC
m7a = lmer(MeanAmp ~ Race * Fix + Trial + (Race*Fix|Subject) + (1|Electrode), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.5 P2_randomEffectSubject_fixedTrial_TBTdat.txt")
# summary(m7)
# "________________________________________________________________________________________________"
# coef(m7)
# "________________________________________________________________________________________________"
# summary(m7a)$AICtab
# sink()

# Add trial as fixed effect (with interactions)
m8 = lmer(MeanAmp ~ Race * Fix * Trial + (Race*Fix|Subject) + (1|Electrode), data = dat)
# fit with FIML in order to get AIC
m8a = lmer(MeanAmp ~ Race * Fix * Trial + (Race*Fix|Subject) + (1|Electrode), data = dat, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/4.6 P2_randomEffectSubject_fixedTrialWithInteractions_TBTdat.txt")
# summary(m8)
# "________________________________________________________________________________________________"
# coef(m8)
# "________________________________________________________________________________________________"
# summary(m8a)$AICtab
# sink()
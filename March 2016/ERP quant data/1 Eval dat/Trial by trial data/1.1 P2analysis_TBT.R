# read in data
path = "./March 2016/ERP quant data/1 Eval dat/Trial by trial data/"
dat = read.delim(paste(path,"TrialByTrialQuantifiedDataWithConditions.txt", sep=""), sep = "\t",header = T)

require(lme4)
require(lmerTest)
require(magrittr)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), data = dat)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]

# Compare to examine effect of subject. Significant, so random intercept by subject adds to model
m1a = lmer(MeanAmp ~ 1 + (1|Electrode), data = dat)
anova(m1,m1a)

# Compare to examine effect of electrode
# enough electrodes to use as grouping variable?? Or just fixed effect?
m1b = lmer(MeanAmp ~ 1 + (1|Subject), data = dat)
anova(m1,m1b)



# Next step: Include fixed effects
m3 = lmer(MeanAmp ~ Race * Fix + Electrode + (1|Subject), data = dat)
summary(m3)
m3a = lmer(MeanAmp ~ Race * Fix + Electrode + (1|Subject), data = dat, REML = F)

# Write to output file
# sink(file = "./March 2016/Model outputs/2.3 P2_Eval_varyingInterceptModel_TBTdat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

m4 = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject), data = dat)
summary(m4)
m4a = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject), data = dat, REML = F)

# Write results to separate file
# sink(file = "./March 2016/Model outputs/2.4 P2_Eval_MaximalModel_TBTdat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

# include effect of trial?

m5 = lmer(MeanAmp ~ Race * Fix + Trial + Electrode + (Race*Fix|Subject), data = dat)
summary(m5)
m5a = lmer(MeanAmp ~ Race * Fix + Trial + Electrode + (Race*Fix|Subject), data = dat, REML = F)
summary(m5a)$AICtab

anova(m4a,m5a)

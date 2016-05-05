# Takes averages for each bin and condition and submits to MLM
require(lme4)
require(lmerTest)
require(magrittr)

# read in data
path = "./March 2016/ERP quant data/1 Eval dat/Trial by trial data/Binned Data/"
dat = read.delim(paste(path,"BinnedQuantifiedData.txt", sep=""), sep = "\t",header = T)

# First, intercept-only model
m1 = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Bin), data = dat)
summary(m1)
# ICC for subject: .40
# ICC for bin:.02


# Next step: Include fixed effects
m3 = lmer(MeanAmp ~ Race * Fix + Electrode + (1|Subject) + (1|Bin), data = dat)
summary(m3)
m3a = lmer(MeanAmp ~ Race * Fix + Electrode + (1|Subject) + (1|Bin), data = dat, REML = F)

# Write to output file
# sink(file = "./March 2016/Model outputs/2.3 P2_Eval_varyingInterceptModel_BinnedDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

m4 = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject) + (Race*Fix|Bin), data = dat)
summary(m4)
m4a = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject) + (Race*Fix|Bin), data = dat, REML = F)

# Write results to separate file
# sink(file = "./March 2016/Model outputs/2.3 P2_Eval_MaximalModel_BinnedDat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

blups1 = coef(m4)$Bin[1]
blups1$Bin = 1:15
plot(blups1$Bin, blups1$`(Intercept)`)

# Model with slopes varying by subject, intercept varying by bin
m5 = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject) + (1|Bin), data = dat)
summary(m5)
m5a = lmer(MeanAmp ~ Race * Fix + Electrode + (Race*Fix|Subject) + (1|Bin), data = dat, REML = F)

# Write results to separate file
# sink(file = "./March 2016/Model outputs/2.3 P2_Eval_slopesVaryBySubject_BinnedDat.txt")
# summary(m5)
# "________________________________________________________________________________________________"
# coef(m5)
# "________________________________________________________________________________________________"
# summary(m5a)$AICtab
# sink()

blups = coef(m5)$Bin[1]
blups$Bin = 1:15
plot(blups$Bin, blups$`(Intercept)`)

# What about include bin as a fixed effect to see if change over time is significant?
# Effect of bin is highly significant (when bin is continuous)
m6 = lmer(MeanAmp ~ Race * Fix + Electrode + Bin + (Race*Fix|Subject), data = dat)
summary(m6)
m6a = lmer(MeanAmp ~ Race * Fix + Electrode + Bin + (Race*Fix|Subject), data = dat, REML = F)

# Write results to separate file
# sink(file = "./March 2016/Model outputs/2.3 P2_Eval_BinAsFixed_BinnedDat.txt")
# summary(m6)
# "________________________________________________________________________________________________"
# coef(m6)
# "________________________________________________________________________________________________"
# summary(m6a)$AICtab
# sink()

# What if bin is categorical instead of continuous?
m7 = lmer(MeanAmp ~ Race * Fix + Electrode + as.factor(Bin) + (Race*Fix|Subject), data = dat)
summary(m7)

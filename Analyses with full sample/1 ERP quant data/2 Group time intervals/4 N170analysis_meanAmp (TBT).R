require(lme4)
require(lmerTest)
require(magrittr)

# Eval task ---------------------------------------------------------------


# read in data
path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
evaldat = read.delim(paste(path, "Quantified data/Eval_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))

# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), data = evaldat)
summary(m1)

# Compare to examine effect of trial- significant
m2a = lmer(MeanAmp ~ 1 + (1|Subject), data = evaldat)
anova(m1,m2a)

m2b = lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode:Subject), data = evaldat)
anova(m2b, m2a)
# lower AIC/BIC for electrode nested within subject


# Next step: Include fixed effects

# Maximal model
m4 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (Race*Fix|Electrode:Subject), data = evaldat)

# sink(file = paste(path, "Model outputs/4.1 N170_Eval_MaximalModel_TBTdat.txt", sep=""))
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# sink()

# Random effects for Subject but only intercept for electrode
m5 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(path, "Model outputs/4.2 N170_Eval_randSlopesSub_randIntElecNestedinSub_TBTdat.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()



# Cat task ---------------------------------------------------------------


# read in data
path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
catdat = read.delim(paste(path, "Quantified data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))


# Random effects for Subject but only intercept for electrode
m6 = lmer(MeanAmp ~ Race * Fix + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

sink(file = paste(path, "Model outputs/5 N170_Cat_randSlopesSub_randIntElecNestedinSub_TBTdat.txt", sep=""))
summary(m6)
"________________________________________________________________________________________________"
coef(m6)
sink()




# Look at tasks together --------------------------------------------------

evaldat$Task = "Eval"
catdat$Task = "Cat"

bothdat = rbind(evaldat, catdat)
bothdat$Task = factor(bothdat$Task)

m7 = lmer(MeanAmp ~ Race * Fix * Task + (Race*Fix|Subject) + (1|Electrode:Subject), data = bothdat)

sink(file = paste(path, "Model outputs/6 N170_Both_randSlopesSub_randIntElecNestedinSub_TBTdat.txt", sep=""))
summary(m7)
"________________________________________________________________________________________________"
coef(m7)
sink()


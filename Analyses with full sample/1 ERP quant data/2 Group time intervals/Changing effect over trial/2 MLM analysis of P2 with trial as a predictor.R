require(lme4)
require(lmerTest)

path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
filepath = "Analyses with full sample/1 ERP quant data/2 Group time intervals/Changing effect over trial/"

# read in cat data
catdat = read.delim(paste(path, "Quantified data/Cat_AllSubs_TBTaverages_noBS_groupP2.txt", sep=""))

# Random effects for Subject but only intercept for electrode
m1 = lmer(MeanAmp ~ Race * Fix * Trial + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

sink(file = paste(filepath,"M2_P2_Cat_TBTdat_TrialEquals0.txt", sep=""))
summary(m1)
"________________________________________________________________________________________________"
coef(m1)
sink()


# read in eval data
evaldat = read.delim(paste(path, "Quantified data/Eval_AllSubs_TBTaverages_noBS_groupP2.txt", sep=""))

# Random effects for Subject but only intercept for electrode
m2 = lmer(MeanAmp ~ Race * Fix * Trial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(filepath,"M1_P2_Eval_TBTdat_TrialEquals0.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Adjust Trial variable to look at fixed effects at the end of eval task
evaldat$Trial512 = evaldat$Trial - 512
# Random effects for Subject but only intercept for electrode

m3 = lmer(MeanAmp ~ Race * Fix * Trial512 + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(filepath,"M1.2_P2_Eval_TBTdat_TrialEquals512.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

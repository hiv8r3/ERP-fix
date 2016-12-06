require(lme4)
require(lmerTest)
require(dplyr)

path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
filepath = "Analyses with full sample/1 ERP quant data/2 Group time intervals/Changing effect over trial/"

# read in cat data
catdat = read.delim(paste(path, "Quantified data/Cat_AllSubs_TBTaverages_noBS_groupP2.txt", sep=""))

catdat$rescaleTrial = catdat$Trial/25.6

# Random effects for Subject but only intercept for electrode
m1 = lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

sink(file = paste(filepath,"M3_P2_Cat_TBTdat_RescaledTrial.txt", sep=""))
summary(m1)
"________________________________________________________________________________________________"
coef(m1)
sink()


# read in eval data
evaldat = read.delim(paste(path, "Quantified data/Eval_AllSubs_TBTaverages_noBS_groupP2.txt", sep=""))
evaldat$rescaleTrial = evaldat$Trial/51.2

# Random effects for Subject but only intercept for electrode
m2 = lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(filepath,"M4_P2_Eval_TBTdat_RescaledTrial.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()




# test simple slopes ------------------------------------------------------

# # Example
# data("Orthodont",package="MEMSS")
# fm2 <- lmer(
#   formula = distance ~ age*Sex + (age|Subject)
#   , data = Orthodont
# )
# newdat2 <- expand.grid(
#   age=c(8,10,12,14)
#   , Sex=c("Female","Male")
#   , distance = 0
# )
# 
# mm2 <- model.matrix(terms(fm2),newdat2)
# newdat2$distance <- predict(fm2,newdat2,re.form=NA)
# #newdat$distance <- mm %*% fixef(fm1)
# pvar2 <- diag(mm2 %*% tcrossprod(vcov(fm2),mm2))
# cmult <- 2 ## could use 1.96
# newdat <- data.frame(
#   newdat,
#   plo = newdat$distance-cmult*sqrt(pvar2),
#   phi = newdat$distance+cmult*sqrt(pvar2)
# )
# #plot confidence
# g0 <- ggplot(newdat, aes(x=age, y=distance, colour=Sex))+geom_point() +
#   g0 + geom_errorbar(aes(ymin = plo, ymax = phi))+
#   labs(title="CI based on fixed-effects uncertainty ONLY")


fm1 <- lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)
newdat <- expand.grid(
  Race = c("Black", "White"), 
  Fix=c("eyes","forehead"),
  rescaleTrial=1:512,
  MeanAmp = 0
)

mm <- model.matrix(terms(fm1),newdat)
newdat$MeanAmp <- predict(fm1,newdat,re.form=NA)
#newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
cmult <- 2 ## could use 1.96
newdat <- data.frame(
  newdat, 
  plo = newdat$MeanAmp-cmult*sqrt(pvar1),
  phi = newdat$MeanAmp+cmult*sqrt(pvar1)
)

newdat$Condition <- paste(newdat$Race, newdat$Fix, sep=", ")

require(ggplot2)
g0 <- ggplot(newdat[newdat$rescaleTrial<20,], aes(x=rescaleTrial, y=MeanAmp, colour=Condition))+geom_line() 
g0 + geom_errorbar(aes(ymin = plo, ymax = phi))+
  labs(title="CI based on fixed-effects uncertainty ONLY")

write.table(newdat, paste(filepath,"confInt.txt", sep=""), row.names=F,sep="\t")

head(coef(fm1))


# from Ed -----------------------------------------------------------------

g0 <- ggplot(evaldat, aes(x=rescaleTrial, y=MeanAmp, colour=Condition)) + geom_line()



# what I’m approximately looking for? --------------------------------------

lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat) %>% 
  summary()


lmer(MeanAmp ~ rescaleTrial + (1|Subject) + (1|Electrode:Subject), # b = .171
     data = evaldat[evaldat$Condition == "Black_eyes",]) %>% 
  summary()

lmer(MeanAmp ~ rescaleTrial + (1|Subject) + (1|Electrode:Subject), # b = .101
     data = evaldat[evaldat$Condition == "Black_forehead",]) %>% 
  summary()

lmer(MeanAmp ~ rescaleTrial + (1|Subject) + (1|Electrode:Subject), # b = .104
     data = evaldat[evaldat$Condition == "White_eyes",]) %>% 
  summary()

lmer(MeanAmp ~ rescaleTrial + (1|Subject) + (1|Electrode:Subject), # b = .103
     data = evaldat[evaldat$Condition == "White_forehead",]) %>% 
  summary()


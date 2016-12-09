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




# test simple slopes for eval ------------------------------------------------------


fm1 <- lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

parms <- fixef(fm1) # fixed parameters from model
vcmat <- vcov(fm1) # variance/covariance matrix of fixed effects

## each row of this matrix defines entries of parms that we want to combine:
Tmat <- matrix(NA,4,8)
Tmat[1,] <- c(rep(0,3),1,0,0,0,0) # weights for estimates for race = 0, fix = 0 (Black-eyes)
Tmat[2,] <- c(rep(0,3),1,0,0,1,0) # weights for estimates for race = 0, fix = 1 (Black-forehead)
Tmat[3,] <- c(rep(0,3),1,0,1,0,0) # weights for estimates for race = 1, fix = 0 (White-eyes)
Tmat[4,] <- c(rep(0,3),1,0,1,1,1) # weights for estimates for race = 1, fix = 1 (White-forehead)

parest <- Tmat %*% parms          # %*% is matrix multiplication. Uses the weights in Tmat to do the subtraction/addition of the estimates for you
newvc <- Tmat %*% vcmat %*% t(Tmat) # shortcut to the usual calculation: Var(x + y) = Var(x) + Var(y) + 2*cov(x,y)
ses <- sqrt(diag(newvc))            # to calculate SE

## final results
fin = cbind(parest, ses) %>% as.data.frame()
names(fin) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE

fin$lbnd = fin$est - 2*fin$ses
fin$ubnd = fin$est + 2*fin$ses

fin = format(fin, digits = 3)

fin$condition = c("Black-eyes", "Black-forehead", "White-eyes", "White-forehead")

write.table(fin, paste(filepath,"Eval_confInt.txt", sep=""), row.names=F,sep="\t")


# test simple slopes for cat ------------------------------------------------------


fm2 <- lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

parms <- fixef(fm2) # fixed parameters from model
vcmat <- vcov(fm2) # variance/covariance matrix of fixed effects

## each row of this matrix defines entries of parms that we want to combine:
Tmat <- matrix(NA,4,8)
Tmat[1,] <- c(rep(0,3),1,0,0,0,0) # weights for estimates for race = 0, fix = 0 (Black-eyes)
Tmat[2,] <- c(rep(0,3),1,0,0,1,0) # weights for estimates for race = 0, fix = 1 (Black-forehead)
Tmat[3,] <- c(rep(0,3),1,0,1,0,0) # weights for estimates for race = 1, fix = 0 (White-eyes)
Tmat[4,] <- c(rep(0,3),1,0,1,1,1) # weights for estimates for race = 1, fix = 1 (White-forehead)

parest <- Tmat %*% parms          # see above for notation
newvc <- Tmat %*% vcmat %*% t(Tmat)
ses <- sqrt(diag(newvc)) 

## final results
fin = cbind(parest, ses) %>% as.data.frame()
names(fin) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE

fin$lbnd = fin$est - 2*fin$ses
fin$ubnd = fin$est + 2*fin$ses

fin = format(fin, digits = 3)

fin$condition = c("Black-eyes", "Black-forehead", "White-eyes", "White-forehead")

write.table(fin, paste(filepath,"Cat_confInt.txt", sep=""), row.names=F,sep="\t")


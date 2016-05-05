# To adjust trigger codes for ev2 files, since E-Prime is marking Black-eyes trials wrong

# Black-forehead: 55
# White-forehead: 56
# Black-eyes: 65
# White-eyes: 66

# do in a loop for each file
for (i in 1:51) {
  dat = read.delim(paste("ERP quant data/ev2 files for cat task/Sub", i, "_rev.ev2", sep = ""), header = F, sep = "\t")

  # rename incorrect responses column
  names(dat)[3] = "response.wrong"
  # create new column for right codes for responses
  dat$response.correct = NA
  # response codes are the same for all targets except Black-eyes (65)
  dat$response.correct[dat$V2 != 65] = dat$response.wrong[dat$V2 != 65]
  # change response codes for Black-eyes trials
  dat$response.correct[dat$V2 == 65 & dat$response.wrong == 1] = 2
  dat$response.correct[dat$V2 == 65 & dat$response.wrong == 2] = 1
  dat$response.correct[dat$V2 == 65 & dat$response.wrong == 3] = 1
  
  # same for accuracy column
  names(dat)[4] = "accuracy.wrong"
  dat$accuracy.correct = NA
  dat$accuracy.correct[dat$V2 != 65] = dat$accuracy.wrong[dat$V2 != 65]
  dat$accuracy.correct[dat$V2 == 65 & dat$accuracy.wrong == 0] = 1
  dat$accuracy.correct[dat$V2 == 65 & dat$accuracy.wrong == 1] = 0
  
  # rearrange order of columns
  dat = dat[,c(1,2,7,8,5,6)]
  write.table(dat, paste("ERP quant data/ev2 files for cat task/Sub", i, "_rev_corrected.ev2", sep = ""), 
              row.names = F, col.names = F, sep = "\t")
}

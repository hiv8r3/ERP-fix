path = "~/Documents/Projects/ERP-fix/R code- ERP-fix/PCA/"
# modifies files exported from Scan to put into matlab for PCA- takes off Nose

elec = c("FZ", "FC3", "FCZ", "FC4", "C3", "CZ", "C4", "CP3", "CPZ", "CP4", "P3", "PZ", "P4",
			"OZ", "FP1", "FP2", "A2", "P8", "TP7", "TP8", "P7", "VEOG", "HEOG", "Nose")

condList = c("BF", "BE", "WF", "WE")	

for (i in c(1:64, 66)) {
	for (k in condList) {
	dat = read.delim(paste(path, "1 Input files from SCAN/Sub", i, "_", k, "_exportForPCA.txt", sep=""), header = F, skip = 16)
	names(dat) = elec
	dat2 = dat[,1:23]
	# for subjects 1-9, add 0 for first digit to keep # of digits the same
	if (i < 10) {
		write.table(dat2, paste(path, "2 Modified in R/Sub0", i, "_", k, 		"_exportForPCA_mod.txt", sep=""), 			sep = "\t", row.names = F)
	} else {
		write.table(dat2, paste(path, "2 Modified in R/Sub", i, "_", k, 		"_exportForPCA_mod.txt", sep=""), 			sep = "\t", row.names = F)
	}	
	}
}



# similar, but also take off HEOG and VEOG

for (i in c(1:64, 66)) {
	for (k in condList) {
	dat = read.delim(paste(path, "1 Input files from SCAN/Sub", i, "_", k, "_exportForPCA.txt", sep=""), header = F, skip = 16)
	names(dat) = elec
	dat2 = dat[,1:21]
	# for subjects 1-9, add 0 for first digit to keep # of digits the same
	if (i < 10) {
		write.table(dat2, paste(path, "2 Modified in R/Sub0", i, "_", k, 		"_exportForPCA_noEOG.txt", sep=""), 			sep = "\t", row.names = F)
	} else {
		write.table(dat2, paste(path, "2 Modified in R/Sub", i, "_", k, 		"_exportForPCA_noEOG.txt", sep=""), 			sep = "\t", row.names = F)
	}	
	}
}

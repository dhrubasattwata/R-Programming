#specdata folder containing 332 csv files in your wd

#Poluution_Mean
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE) 
  
  dat <- data.frame()
  
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  mean(dat[, pollutant], na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")


#Complete.R
complete <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  
  for (i in id) {
    moni_i <- read.csv(files_full[i])
    nobs <- sum(complete.cases(moni_i))
    tmp <- data.frame(i, nobs)
    dat <- rbind(dat, tmp)
  }
  
  colnames(dat) <- c("id", "nobs")
  dat
}


cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)


set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])



#Corr.r
corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


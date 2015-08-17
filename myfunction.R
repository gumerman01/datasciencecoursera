myfunction <- function(x) {
	y <- rnorm(100)
	mean(y)
}

second <- function(x) {
	x + rnorm(length(x))
}

add2 <- function(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

columnMean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}

pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
    # Set working directory as the place where the files are located
    homedir <- getwd()
    mondir <- paste("/Users/rodrigblanco/test-repo/", directory, sep="")
    setwd(mondir)
    # Insert the information from all monitors into one big dataframe
    for(monitorid in id) { 
        if (monitorid < 10) {
            monitorid <- paste("00", monitorid, sep="")
        } else if (monitorid < 100) {
            monitorid <- paste("0", monitorid, sep="")
        } 
        monitor <- paste(monitorid, "csv", sep=".")
        
        if (!exists("dataset")){
            dataset <- read.table(monitor, header = TRUE, sep = ",")
        }
        else {
            temp_dataset <-read.table(monitor, header=TRUE, sep=",")
            dataset<-rbind(dataset, temp_dataset)
            rm(temp_dataset)
        }
    }
    pollutantNum <- y<-if(pollutant=="nitrate"){ 3
    }else{ 2
    }
    setwd(homedir)
    # Eliminate Missing values
    stripNAs <- is.na(dataset[pollutantNum])
    pollutantList <- dataset[pollutantNum][!stripNAs]
    # Return Mean
    mean(pollutantList)
}

complete <- function(directory = "specdata", id = 1:332) {
    homedir <- getwd()
    mondir <- paste("/Users/rodrigblanco/test-repo/", directory, sep="")
    setwd(mondir)
    # Insert the information from all monitors into one big dataframe
    for(monitorid in id) { 
        if (monitorid < 10) {
            monitor <- paste("00", monitorid, sep="")
        } else if (monitorid < 100) {
            monitor <- paste("0", monitorid, sep="")
        } else {
            monitor <- as.character(monitorid)
        }
        monitor <- paste(monitor, "csv", sep=".")
        temp_dataset <-read.table(monitor, header=TRUE, sep=",")
        good <- complete.cases(temp_dataset)
        datasetComplete <- temp_dataset[good,]
        dimensionDataset <- dim(datasetComplete)[1]
        if (!exists("dataset")){
            dataset <- data.frame(id = monitorid, nobs = dimensionDataset)
        }
        else {
            newRow <-data.frame(id = monitorid, nobs = dimensionDataset)
            dataset<-rbind(dataset, newRow)
            rm(newRow)
        }    
        rm(temp_dataset)
    }
    setwd(homedir)
    dataset
}

corr <- function(directory = "specdata", threshold = 0) {
    homedir <- getwd()
    mondir <- paste("/Users/rodrigblanco/test-repo/", directory, sep="")
    setwd(mondir)
    id <- 1:332
    x <- vector(mode="numeric", length=0)
    
    # Insert the information from all monitors into one big dataframe
    for(monitorid in id) { 
        if (monitorid < 10) {
            monitor <- paste("00", monitorid, sep="")
        } else if (monitorid < 100) {
            monitor <- paste("0", monitorid, sep="")
        } else {
            monitor <- as.character(monitorid)
        }
        monitor <- paste(monitor, "csv", sep=".")
        temp_dataset <-read.table(monitor, header=TRUE, sep=",")
        good <- complete.cases(temp_dataset)
        datasetComplete <- temp_dataset[good,]
        dimensionDataset <- dim(datasetComplete)[1]
        if (dimensionDataset > threshold) {
            datasetSulfate <- datasetComplete[2]
            datasetNitrate <- datasetComplete[3]
            datasetCorr <- cor(datasetSulfate,datasetNitrate)
            x <- c(x,datasetCorr)
        }
    }
    setwd(homedir)
    x
}
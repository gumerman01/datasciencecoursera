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
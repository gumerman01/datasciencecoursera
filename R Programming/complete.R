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
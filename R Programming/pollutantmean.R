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

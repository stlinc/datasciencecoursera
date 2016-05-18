# pollutantmean.R
pollutantmean <- function(directory, pollutant, id = 1:332) {
    # directory: location of the CSV files
    # pollutant: "sulfate" or "nitrate"
    # id: monitor ID

    #initialize data_set
    data_set <- numeric()
    # loop through csv files in id range
    # directory <- "specdata"
    # pollutant <- "nitrate"
    # id <- 23
    for (i in id) {
        # construct file path
        file_path <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep="")
        # load dataset to temp_dataset
        temp_dataset <- read.csv(file_path)
        # rbind tem_dataset to dataset
        data_set <- rbind(data_set, temp_dataset)
    }
    # compute pollutant mean
    round(mean(data_set[,pollutant], na.rm=TRUE), 3)

}

# complete.R
complete <- function(directory, id = 1:332) {
    # directory: location of the CSV files
    # id: monitor ID

    dataframe = data.frame()  ## initializing the dataframe 
    # directory <- "specdata"
    # id <- 23
    #Looping thru the directory's files specified in the 'id' argument
    for (i in id) {
        # construct file path
        file_path <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep="")
        # load dataset to temp_dataset
        temp_dataset <- read.csv(file_path)
        nobs <- sum(complete.cases(temp_dataset))
        dataframe = rbind(dataframe, c(i,nobs)) # Each row contains the monitor ID,
        # and its total complete observed cases (no rows containg NAs)
    }

    #dataframe = data.frame(dataframe)  # from matix to data frame
    names(dataframe) = c('id', 'nobs') # set the column names of the data frame
    return (dataframe)
}

# corr.R
corr <- function(directory, threshold = 0) {
    corList= NULL ## initializing the correlation vector
    #Looping thru ALL the directory's files
    for (i in 1:332) {
        # construct file path
        file_path <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep="")
        # load dataset to temp_dataset
        temp_dataset <- read.csv(file_path)
        # remove NAs
        temp_dataset <- temp_dataset[complete.cases(temp_dataset),]
        if (nrow(temp_dataset) > threshold) {
            corList <- c(corList, cor(temp_dataset[,2], temp_dataset[,3]))
        }
    }
    # Check if no location has complete cases greater than the threshold.
    if (is.null(corList)) {
        corList <- 0
    }
    return (corList)
}

# Quiz
# Q 1. What value is returned by the following call to pollutantmean()? You should round your output to 3 digits.
pollutantmean("specdata", "sulfate", 1:10)

# A: 4.064

# Q 2. What value is returned by the following call to pollutantmean()? You should round your output to 3 digits.
pollutantmean("specdata", "nitrate", 70:72)

# A: 1.706

# Q 3. What value is returned by the following call to pollutantmean()? You should round your output to 3 digits.
pollutantmean("specdata", "sulfate", 34)

# A: 1.477

# Q 4. What value is returned by the following call to pollutantmean()? You should round your output to 3 digits.
pollutantmean("specdata", "nitrate")

# A: 1.703

# Q 5. What value is printed at end of the following code?
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

# A: 228 148 124 165 104 460 232

# Q 6. What value is printed at end of the following code?
cc <- complete("specdata", 54)
print(cc$nobs)

# A: 219

# Q 7. What value is printed at end of the following code?
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

# A: 711 135  74 445 178  73  49   0 687 237

# Q 8. What value is printed at end of the following code?
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

# A: 0.2688  0.1127 -0.0085  0.4586  0.0447

# Q 9. What value is printed at end of the following code?
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

# A: 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969

# Q 10. What value is printed at end of the following code?
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

# 1.0000 -0.0190  0.0419  0.1901 #1st number is not right, because there is no location whose number of complete cases is greater than 2,000. Correct answer should be 0.

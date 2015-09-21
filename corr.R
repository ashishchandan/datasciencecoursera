corr <- function(directory, threshold = 0) {
  # read all files from the directory specified and get all complete cases
  datafiles <- list.files(directory, pattern="*.csv", full.names=TRUE)
  my_data <- lapply(datafiles, read.csv)
  my_data <- do.call(rbind, my_data)
  my_data <- my_data[complete.cases(my_data), ]
  
  ## create vector to hold results
  v <- numeric()
  
  ## get total complete cases per id
  for (j in 1:length(datafiles)) {
    data <- subset(my_data, my_data[,4]==j) 
    numrows <- nrow(data)
    
    ## if total > threshold
    ## calculate correlation between sulfate and nitrate
    if (numrows > threshold) {
      c <- cor(data$sulfate, data$nitrate)
      c <- round(c, digits=4)
      
      ## add results to vector
      v <- c(v, c)
    }
  }
  
  return(v)
}

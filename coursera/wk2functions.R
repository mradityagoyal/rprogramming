loadAllFiles <- function(dir = "specdata"){
  if(is.na(file.info(dir)$isdir) || !file.info(dir)$isdir){
    print("not a dir")
    return 
  }
  all_files <- list.files(dir, full.names = TRUE)
  for(f in all_files){
    if(exists("readings")){
      readings <- rbind(readings, read.csv(f))  
    }else{
      readings <- read.csv(f)
    }
    
  }
  readings
}


pollutantmean <- function(directory = "specdata", pollutant = "nitrate", id = 1:332){
  all_data <- loadAllFiles(directory)
  sub_set <- all_data[which(all_data$ID %in% id),]
  mean(sub_set[[pollutant]], na.rm = TRUE)
}

complete <- function(directory = "specdata", id = 1:332){
  data <- loadAllFiles(directory)
  
  completes <- vector()
  for(i in id){
    if(exists("comp")){
      comp <- rbind(comp, cbind(i, sum(complete.cases(data[which(data$ID==i),]))))
    }else{
      comp <- cbind(i, sum(complete.cases(data[which(data$ID==i),])))
    }
  }
  names <- c("id", "nobs")
  colnames(comp) <- names
  comp
}

corr <- function(directory = "specdata", threshold = 0){
  complete_stuff <- complete(directory)
  ids <- complete_stuff[which(complete_stuff[, "nobs"] > threshold)]
  data <- loadAllFiles(directory)
  for(i in ids){
    sub_data <- data[which(data$ID==i & complete.cases(data)),]
    if(exists("cor_result")){
      cor_result <- rbind(cor_result, cor(sub_data$sulfate, sub_data$nitrate))
    }else{
      cor_result <- cor(sub_data$sulfate, sub_data$nitrate)
    }
  }
  if(exists("cor_result")){
    res <- cor_result
  }else{
    res <-numeric()
  }
  res
}
pollutantmean2 <- function(directory, id){
    data3 <- c()
    for(i in id){
        indexVal <- toString(i)
        if(i < 100){
            s <- "0"
            s2 <- "00"
            if(i < 10){
                #i <- toString(i)
                i <- paste(s2, indexVal, sep = "")
            }else if(i > 9){
                # i <- toString(i)
                i <- paste(s , indexVal , sep = "")
            }
        }else{
            i <- indexVal
        }
        id.name <- paste(directory,"/",i, ".csv", sep = "")    
        data <- read.csv(id.name) #data frame of around 300 rows and 4 colounms
        data2 <- data[complete.cases(data),]
        data4 <- nrow(data2)
        data3 <- c(data3, data4)
    }
    ks <- (data3)    
}

complete2 <- function(directory,id = 1:332){
    ks <- pollutantmean2(directory, id)
    complete_fils <- data.frame(id = id, nobs = ks)
    complete_fils
}

pollutantmean3 <- function(directory,id){
    data4 <- c()
    for(i in id){
        indexVal <- toString(i)
        if(i < 100){
            s <- "0"
            s2 <- "00"
            if(i < 10){
                #i <- toString(i)
                i <- paste(s2, indexVal, sep = "")
            }else if(i > 9){
                # i <- toString(i)
                i <- paste(s , indexVal , sep = "")
            }
        }else{
            i <- indexVal
        }
        id.name <- paste(directory,"/",i, ".csv", sep = "")    
        data <- read.csv(id.name) #data frame of around 300 rows and 4 colounms
        data <- data[complete.cases(data),]
        data1 <- data[,2]
        data2 <- data[,3]
        data3 <- cor(data1, data2)
        data4 <- c(data4, data3)
    }
    data4
}
    

corr <- function(directory,threshold = 0){
    file_name <- c()
    data_corr <- complete2(directory)
    for(i in 1:332){
        if(data_corr[i,2] > as.numeric(threshold)){
            file_name <- c(file_name, i) #contains all the file names
        }
    }
    file_name <- as.numeric(file_name)
    final <- pollutantmean3(directory,file_name)
    final
}















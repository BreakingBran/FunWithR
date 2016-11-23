


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

complete <- function(directory,id = 1:332){
    ks <- pollutantmean2(directory, id)
    complete_fils <- data.frame(id = id, nobs = ks)
    complete_fils
}




    


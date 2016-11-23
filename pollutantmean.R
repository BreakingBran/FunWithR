
pollutantmean <- function(directory, pollutant, id = 1:332){
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
        if(pollutant == "sulfate"){
            pollutant <- 2
        }else if(pollutant == "nitrate"){
            pollutant <- 3
        }
        raw_data <- data[,pollutant]
        bad <- is.na(raw_data) #subset of all the bad rows in desired colunm 
        data2 <- raw_data[!bad]
        data3 <- c(data3 , data2)
        }
    mean(data3)
    }

#hiii


if(4 < 5){
    print("Hello")
}else{
    print("Why")
}



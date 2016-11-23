
# makeCacheMatrix makes a list with three functions that lets it return the matrix, return the inverse of the matrix
# and set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                 #sets inverse to Nothing
    get <- function() x             #used later in cacheSolve so that it can create an inverse
    set_inverse <- function(solved_inverse) inverse <<- solved_inverse #sets the inverse value after cacheSolve creates it
    get_inverse <- function() inverse #gives the inverse value to the variable invese in the other function when called
    list(get = get, set_inverse = set_inverse, get_inverse = get_inverse) #makes this entire function a giant list
}

# cacheSolve checks to see if the inverse exists, if it does it returns it, if not it solves it and sets it in makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()  #gets the value of the inverse from the other function, if this is the first time the matrix is called it NULL
    if(!is.null(inverse)){      # checks to see if the value of inverse is NULL if not it just returns it
        message("Please Wait as we get your cached data")
        return(inverse)
    }
    data <- x$get()         # Gets the matrix data from the other function
    inverse1 <- solve(data) # Stores the solved version of the matrix
    x$set_inverse(inverse1) #Stores the solved matrix back in the other function
    inverse1                #This one's is self explanatory, hope you enjoyed reading my code  
        
}
















death_variable <- NULL ############################################################
if(cause_of_death == "heart failure"){
    death_variable <- 17
}else if(cause_of_death == "heart attack"){
    death_variable <- 11                            #This decides what disease to look for and if it exists
}else if(cause_of_death == "pneumonia"){
    death_variable <- 23
}else{
    stop("invalid outcome")
}                       ##############################################################
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
names_of_states <- outcome[,7]
if(!state_name %in% names_of_states ){           #Checks to see if the state exists
    stop("invalid state")
}                       ###################################################################
if(num == "best"){
    answer <- best(state_name, cause_of_death)
    answer
    stop()
}
state_results <- split(outcome, outcome$State) # Splits the data frame into states
desired_state_results <- as.data.frame(state_results[state_name]) # Takes the desired state
desired_state_results[,death_variable] <- as.numeric(desired_state_results[,death_variable]) #Turns it into numbers
Hospital_Name <- c() #Makes a hospital list
minim <- NULL       #Sets the minnimum to nothing
row_number <- c()
Sorted_Hospital_Names <- c()
if(num == "worst"){
    num <- nrow(desired_state_results)
    for(values in 1:nrow(desired_state_results)){
        if(is.null(minim)){ 
            minim <- desired_state_results[values,death_variable]
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }else if(is.na(desired_state_results[values,death_variable])){next()
        }else if(minim < desired_state_results[values,death_variable]){
            minim <- desired_state_results[values,death_variable]
            Hospital_Name <- c()
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }else if(minim == desired_state_results[values,death_variable]){
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }
    }
    Sorted_Hospital_Names <- sort(Hospital_Name)
    Sorted_Hospital_Names[1]
    stop()
}
Sorted_Hospital_Names2 <- c()
for(numbers in 1:num){
    if( numbers > 1){
        desired_state_results <- desired_state_results2
    }
    for(values in 1:nrow(desired_state_results)){
        if(is.null(minim)){ 
            minim <- desired_state_results[values,death_variable]
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }else if(is.na(desired_state_results[values,death_variable])){next()
        }else if(minim > desired_state_results[values,death_variable]){
            minim <- desired_state_results[values,death_variable]
            Hospital_Name <- c()
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }else if(minim == desired_state_results[values,death_variable]){
            Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
        }
    }
    Sorted_Hospital_Names <- sort(Hospital_Name)
    Sorted_Hospital_Names <- Sorted_Hospital_Names[1]
    row_number <- which(desired_state_results == Sorted_Hospital_Names)
    Sorted_Hospital_Names2 <- c(Sorted_Hospital_Names2,Sorted_Hospital_Names[1])
    Sorted_Hospital_Names <- c()
    desired_state_results2 <- desired_state_results[-c(row_number),]
}
Sorted_Hospital_Names2[num] 
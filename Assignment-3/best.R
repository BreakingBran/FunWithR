best <- function(name_of_state, cause_of_death){
    death_variable <- NULL 
    if(cause_of_death == "heart failure"){
        death_variable <- 17
    }else if(cause_of_death == "heart attack"){
        death_variable <- 11
    }else if(cause_of_death == "pneumonia"){
        death_variable <- 23
    }else{
        stop("invalid outcome")
    }
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    names_of_states <- outcome[,7]
    if(!name_of_state %in% names_of_states ){ 
        stop("invalid state")
    }
    state_results <- split(outcome, outcome$State) # Splits the data frame into states
    desired_state_results <- as.data.frame(state_results[name_of_state]) # Takes the desired state
    desired_state_results[,death_variable] <- as.numeric(desired_state_results[,death_variable])
    Hospital_Name <- c()
    minim <- NULL
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
    Sorted_Hospital_Names[1]
}
source("best.R")

row_number <- c()

worst <- function(name_of_state, cause_of_death){
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
        }else if(is.na(desired_state_results[values,death_variable])){
            next()
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
}

best2 <- function(name_of_state, cause_of_death, loops_update, counter, num){
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
    internal_row_number <- c()
    loops <- 1:nrow(desired_state_results)
    if(num > nrow(desired_state_results)){
        NA
    }
    else{
        if(counter > 1){ loops <- loops[-c(loops_update)]}
        for(values in loops){
            if(is.null(minim)){ 
                minim <- desired_state_results[values,death_variable]
                Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
                internal_row_number <- c(internal_row_number,values)
            }else if(is.na(desired_state_results[values,death_variable])){
                next()
            }else if(minim > desired_state_results[values,death_variable]){
                minim <- desired_state_results[values,death_variable]
                Hospital_Name <- c()
                Hospital_Name <- c(Hospital_Name,desired_state_results[values,2])
                internal_row_number <- c()
                internal_row_number <- c(internal_row_number,values)
            }else if(minim == desired_state_results[values,death_variable]){
                name_of_hospital <- desired_state_results[values,2]
                Hospital_Name <- c(Hospital_Name,name_of_hospital)
                internal_row_number <- c(internal_row_number, values)
            }
        }
        Sorted_Hospital_Names <- sort(Hospital_Name)
        row_number <<- internal_row_number
        Sorted_Hospital_Names
    }
}


rankhospital <- function(state_name, cause_of_death, num = "best"){
    checker <- 0
    if(num == "best"){
        answer <- best(state_name, cause_of_death)
        answer
    }
    else if(num == "worst"){
        answer <- worst(state_name, cause_of_death)
        answer
    }
    else{
        list_of_hospitals <- c()
        minim <- NULL
        counter <- 0
        row_number <<- c()
        loops_update <- c()
        Hospital_Names_Checker <- c()
        for(values in 1:num){
            counter <- counter + 1
            loops_update <- c(loops_update, row_number)
            best2_answer <- best2(state_name,cause_of_death,loops_update,counter, num)
            if(is.na(best2_answer)){
                checker <- 1
                break
            }
            else if(checker == 0){
            list_of_hospitals <- c(list_of_hospitals,best2_answer)
            }
        }
        if(checker == 0){list_of_hospitals[num]}
        else{NA}
    }
    
    
}






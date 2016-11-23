source("rankhospital.R")

for(values in 1:30){
    number <- ass[values]
    ass <- rankhospital("TX","heart failure",number)
}

ass
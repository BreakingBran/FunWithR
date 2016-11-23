makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

ass <- 1:100
makeVector(ass)



makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL #sets inverse to Nothing
    get <- function() x
    set_inverse <- function(solved_inverse) inverse <<- solved_inverse
    get_inverse <- function() inverse
    list(get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

cacheSolve <- function(x){
    inverse <- x$get_inverse()
    if(!is.null(inverse)){
        message("Please Wait as we get your cached data")
        return(inverse)
    }
    data <- x$get()
    inverse1 <- solve(data)
    x$set_inverse(inverse1)
    inverse1
}
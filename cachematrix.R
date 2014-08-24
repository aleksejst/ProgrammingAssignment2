## Hi
## The below two functions are meant to facilitate inverting matricies by 
## caching the already known results to save time by not making 
## the same calculation twice

## makeCacheMatrix - retruns a list of values that can be used later

## cacheSolve - manipulates values from the makeCacheMatrix to determine if
## a new calculation is required or if a cached value exists, thus determining 
## the source of the function output


makeCacheMatrix <- function(x = matrix(), dimension1, dimension2) {
    ## This function creates a special 
    ## "matrix" object that can cache its inverse.
    
    s <- NULL   ## 'm' stores result of solve, 
    ## this line resets it each time the function is run
    
    x<-matrix(x, nrow = dimension1, ncol = dimension1)            
    ## formats 'x' with proper dimensions
    
    get <- function() x                     ## original value
    setslv <- function(solve) s <<- solve   ## stores superassigned solve value 
    getslv <- function() s                  ## returnes cached value
    
    list(get = get, setslv = setslv, getslv = getslv)    
    ## makes all listed available for external access
}


cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the 
    ## special "matrix" returned by makeCacheMatrix above
    
    s <- x$getslv()         
    ## accesses 'x' and gets solve
    
    if(!is.null(s)) {       
        message("getting cached data")
        return(s)    
    }
    ## checks if presolved results are available
    ## and outputs them if any
    
    data <- x$get()         ## provides values for calculations
    s <- solve(data, ...)   ## runs solve
    x$setslv(s)             ## stores solve output
    s                       ## returns the solve value
} 


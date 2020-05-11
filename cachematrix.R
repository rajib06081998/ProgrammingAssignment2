## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                                                     ## Initialize the inverse with NULL
        set <- function(y){                                                             ## Define the set function
        x <<- y                                                                         ## Updates x if there is a new matrix
        inv <<- NULL                                                                    ## set inverse value to NULL
        }
        get <- function() x                                                             ## Define the get function
        setInverse <- function(inverse) inv <<- inverse                                 ##Sets the value of inv in the parent environment
        getInverse <- function() inv                                                    ##Gets the value of inverse 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    ## To access these above declared functions with $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()               ## Gets the inverse
        if(!is.null(inv)){                  ## Checks if the inverse is NULL or not
        message("Cached Inverse Recovered")     
        return(inv)                         ## Returs the inverse
        }
        mat <- x$get()                      ## Gets the matrix
        inv <- solve(mat,...)               ## Calculates the inverse
        x$setInverse(inv)                   ## Sets the inverse value
        inv                                 ## Returs the inverse
}

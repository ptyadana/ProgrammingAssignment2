## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## Intialize the matrix property
    m <- NULL
    
    ## Set the value of matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## Get the value of matrix
    get <- function() x
    
    
    ## Set the inverse of given matrix
    setInverse <- function(inverse) m <<- inverse
    
    ## Return the inverse of given matrix
    getInverse <- function() m
    
    
    ## Return the list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the inverse of matrix 'x' and set it to 'm'
    m <- x$getInverse()
    
    ## Check whether 'm' has value already set, if have return the value of 'm' which is inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, get the original input matrix
    data <- x$get()
    
    ## Calculate the inverse using built in matrix calculation function
    m <- solve(data) %*% data
    
    ## Set the calculated inverse value to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}

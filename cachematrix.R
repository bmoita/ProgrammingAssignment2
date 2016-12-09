
## The makeCacheMatrix function creates a special "matrix" which is a list
## containing functions to set and cache the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL                       # "cleans" cache to default value NULL
        
        set <- function(y) {
                x <<- y                 # cache the input matrix
                m <<- NULL              # "cleans" cache to default value NULL
        }
        get <- function() x
        setinverse <- function(inverse) 
                m <<- inverse           # caches the inverted matrix x
        getinverse <- function() m
        list(set=set, get=get,          # creates a list for the functions
             setinverse=setinverse, 
             getinverse=getinverse)
}


## CacheSolve function retrieves cached inverse matrix x if already calculated
## If not computes the inverse of matrix x.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()             # checks for cached inverted matrix,
        if(!is.null(m)) {               # returns it if cached, signalling it.
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()                 # If no cached data, calculates the 
        m <- solve(data, ...)           # inverted matrix by setinverse function
        x$setinverse(m)
        m                               # returns inverted matrix
}

## to test run the above script first and after define a squared non singular
## matrix on the console:
##
## > test <- makeCacheMatrix(matrix(data=c(1,1,0,1), nrow=2, ncol=2))
##
## confirm the creation of the matrix by running > teste$get()
##
## retrieve the inverted matrix
## > cacheSolve(test)

# The following is a Matrix inversion project for Coursera. 
# The idea is to codify a sophisticated computation so R can optimize it. 
#
# The following is the text of the assignment:
#"                                                   
# Matrix inversion is usually a costly computation and their may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here). Your assignment is to 
# write a pair of functions that cache the inverse of a matrix.
#
# Write the following functions:
#  1.makeCacheMatrix: This function creates a special "matrix" object that can cache 
#    its inverse.
#  2.cacheSolve: This function computes the inverse of the special "matrix" returned 
#    by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
#    has not changed), then the cachesolve should retrieve the inverse from the cache.
#
# Computing the inverse of a square matrix can be done with the solve function in R. For 
# example, if X is a square invertible matrix, then solve(X) returns its inverse.
#"
# Copying some of the lanuage from the example given the following functions 
# are in the makeCacheMatrix. THe end result is a list containing function.
# 
# 1. set - set the value of the matrix. indicator <- NULL
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL 
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 
# here is an example that the above is modelled after.
##makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 
# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 

 
# This function assumes that the matrix is always invertible. 

    inv <- x$getinverse() 
    if(!is.null(inv)) { 
        message("getting cached data.") 
        return(inv) 
    } 
    data <- x$get() 
    inv <- solve(data) 
    x$setinverse(inv) 
    inv 
} 

## 
##cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}

        

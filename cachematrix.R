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
#
# Basically building list containing functions to 
# pass in the cacheSolve function.
# 
# getMatrix - get the value of the matrix passed in
# setGlobal -  set the value of inverse of the matrix globally
# getInverted -  get the value of the global inverse of the matrix value
# 
makeCacheMatrix <- function(x = matrix()) {
 
    invertedMatrix <- NULL 
    getMatrix <- function() x 
    setGlobal <- function(inverted) invertedMatrix <<- inverted 
    getInverted <- function() invertedMatrix 
    list(getMatrix=getMatrix, setGlobal=setGlobal, getInverted=getInverted) 
} 
#
# 
# The following function takes in the list of function and either 1: inverts a square matrix 
# and sets the value global, or 2: returns the previously set global value. There is not 
# checking but the matrix needs to be "square" i.e. 2x2, 3x3, etc. as per the solve(). 
# 
cacheSolve <- function(x, ...) {
    invertedMatrix <- x$getInverted() 
    if(!is.null(invertedMatrix)) { 
        print("Already set, getting cache") 
        return(invertedMatrix) 
    } 
    mtrx <- x$getMatrix() 
    invertedMatrix <- solve(mtrx) 
    x$setGlobal(invertedMatrix) 
    invertedMatrix 
} 
###
###  Testing Results ###
###
#> source("cachematrix.R")
#> w <- matrix(c(1,2,2,1), ncol = 2 , nrow = 2)
#> w
#     [,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> ww <- makeCacheMatrix(w)
#> cacheSolve(ww)
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(ww)
#[1] "Already set, getting cache"
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> w <- matrix(c(1,2,2,1,2,3), ncol = 2 , nrow = 3)
#> w
#     [,1] [,2]
#[1,]    1    1
#[2,]    2    2
#[3,]    2    3
#> ww <- makeCacheMatrix(w)
#> cacheSolve(ww)
#Error in solve.default(mtrx) : 'a' (3 x 2) must be square
#> 

        

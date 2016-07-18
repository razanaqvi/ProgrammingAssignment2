## Put comments here that give an overall description of what your
## functions do
## RAZA NAQVI: I've tried to comment the lines of the functions to describe the logic.

#NOTE: See sample usage and output at the bottom of this file after the function definitions.


## Write a short comment describing this function
#The makeCacheMatrix function creates a special "matrix" object that stores the value of the matrix 
#and its inverse. If the inverse has already been computed, it can be retrieved using getCacheInverse().
#If the inverse hasn't been calculated yet, the getCacheInverse() function returns NULL.

makeCacheMatrix <- function(x = matrix()) {
        #set the internal value of inverse, i
        i <- NULL
        set <- function(y) {
                #set the new value of matrix x and assign NULL to i
                message("Setting the value of the matrix")
                x <<- y
                i <<- NULL
        }
        get <- function() {
                message("Getting the value of the matrix")
                x #return the matrix x
        }
        setCacheInverse <- function(inverse) {
                message("Setting the value of the matrix inverse")
                i <<- inverse #set the value of the inverse
        }
        getCacheInverse <- function() {
                message("Getting the value of the matrix inverse")
                i #return the value of the matrix nverse
        }
        
        list(set = set, get = get, setCacheInverse = setCacheInverse, getCacheInverse = getCacheInverse)
}


## Write a short comment describing this function
#cacheSolve takes a 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #Get the existing value of the matrix inverse from the cache (Note: it could be NULL, 
        #if it hasn't been set yet)
        i <- x$getCacheInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #If we are here, then the cache contains NULL. Get the matrix and compute its inverse.
        data <- x$get()
        i <- solve(data, ...) #solve the inverse of the matrix
        
        #Set the cache value to the computed matrix inverse
        x$setCacheInverse(i)
        
        #return the computed matrix inverse
        i
}

# > m <- makeCacheMatrix(matrix(c(7,-2,-6,2), nrow = 2, ncol = 2, byrow = TRUE))
# > m$get()
# Getting the value of the matrix
# [,1] [,2]
# [1,]    7   -2
# [2,]   -6    2
# > m$getCacheInverse()
# Getting the value of the matrix inverse
# NULL
# > cacheSolve(m)
# Getting the value of the matrix inverse
# Getting the value of the matrix
# Setting the value of the matrix inverse
# [,1] [,2]
# [1,]    1  1.0
# [2,]    3  3.5
# > m$getCacheInverse()
# Getting the value of the matrix inverse
# [,1] [,2]
# [1,]    1  1.0
# [2,]    3  3.5
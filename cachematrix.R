## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function accepts a matrix and creates an object x; it then initializes
## four functions that are stored in a list.
## It also creates the object m, which is later used to define the inverse of 
## the matrix x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) m <<- solved
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function solves the inverse of x as created in makeCacheMatrix, and 
## using setinverse() defined in MakeCacheMatrix, stores it in cache.
## If the inverse has already been solved, it returns the result from the cache.
## Otherwise, it accepts a new matrix from the set() function.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
                data <- x$get()
                m <- solve(data,...)
                x$setinverse(m)
                m
                
        
}

## Return a matrix that is the inverse of 'x'
# > t <- matrix(1:4, 2,2)
# > t1 <- makeCacheMatrix(t)
# > cacheSolve(t1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(t1)
# getting cached inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5




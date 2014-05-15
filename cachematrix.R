#  The makeCacheMatrix function creates a list of functions which can then be called to set and get the inverse of 
#  the matrix. The function cacheSolve computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
#  then cachesolve will retrieve the inverse from the cache.
# 

## This  function, makeCacheMatrix creates and returns a list containing functions to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    CreateInverse <- function(solve) m <<- solve
    GetInverse <- function() m
    list(set = set, get = get,
         CreateInverse = CreateInverse,
         GetInverse = GetInverse)   
}


# This function starts by retrieving the Inverse from the cache by calling GetInverse(). If GetInverse()
# returns a null value, then the inverse matrix has to be created. This is done by calling the get()
# method to retrieve the original matrix and then calling the solve function to create the inverse
# matrix. This is then saved to the cache by calling CreateInverse(m) and passing the inverse matrix
# as the parameter. Finally the inverse matrix m is returned.
cacheSolve <- function(x, ...) {
    m <- x$GetInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$CreateInverse(m)
    m
}

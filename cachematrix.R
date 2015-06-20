## Function makeCacheMatrix gets matrix as inputs and it stores inside four different function that can be returned 
## Function set sets new matrix to perform inversion on it and nullifies previous stored result
## Function get retrieves the stored input matrix
## Function setinv sets the inverted matrix inside the cached variable
## Function getinv retrieves the stored inverted matrix


makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv<- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Cachesolve approaches to inverting the matrix, but first tries to retrieve stored ready result from cache
## if there is ready result in cache, it is returned
## otherwise,  it gets the data via get function, inverts it, sets it into the cache memory and prints it on the screen

## In order to use it correctly, x argument should be created by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

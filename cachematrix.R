## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. In this assignment we are required to 
## write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. 

## myMatrix <- makeCacheMatrix(matrix(c(1,5,0,0,1,0,4,0,1),nrow = 3))

## results in an object, myMatrix, that contains four functions:
## set(), get(), setinv(), and getinv(). It also includes the two
## data objects, x and inv.

## myMatrix$get(), gives us the above matrix (i.e. myMatrix)

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

## cacheSolve(myMatrix) provide the cached inverse of myMatrix.

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

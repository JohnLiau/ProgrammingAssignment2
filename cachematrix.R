## This function set and get the value of the matrix
## and then set and get the value of inverse matrix

## The input is a matrix and the output is a list containing 4 objects

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes inverse matrix of the input matrix of makeCacheMatrix function
## or reuses the inverse matrix of the input matrix if the inverse matrix is available

## The input is the list preprocessed by makeCacheMatrix
## and the output is the inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

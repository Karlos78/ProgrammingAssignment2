
## This file contains two functions - the first (makeCacheMatrix) creates four further functions (in a list)
## these functions are used to perform operations on a matrix


makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    ## "set" the value of the argument matrix and its inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## retrieve the value of the argument matrix
    get <- function() x

    ## set the value of the matrix inverse, using the 'solve' function
    setinv <- function (solve) inv <<- solve

    ## retrieve the value of the inverse
    getinv <- function() inv

    ## output the list of the four functions created
    list (set = set, get = get, setinv = setinv, getinv = getinv)

}

## The second function (cacheSolve) uses the functions created by makeCacheMatrix to calculate the matrix inverse

cacheSolve <- function(x, ...) {

    ## retrieve the matrix inverse from the cache
    inv <- x$getinv()

    ## if the inverse exists then simply return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## if the inverse does not exist then retrieve the original matrix
    matrix <- x$get()

    ## calculate the inverse
    inv <- solve(matrix, ...)

    ## set the value of the inverse in the cache
    x$setinv(inv)

    ## return the inverse matrix
    inv
}

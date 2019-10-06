##These funtions are made to cache a matrix and its inverse so R dont need to
##repeatedly calculate it

##This function creates the cache

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<-y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = get,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Here we compute the inverse or retrive the value if its already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("The value was already stored and here it is")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

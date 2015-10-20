## Create a matrix object to cache the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
      
    }
    get <- function() x
    setmatrix <- function(solve) i <<- solve
    getmatrix <- function() i
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    i <- x$getmatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setmatrix(i)
    i
}
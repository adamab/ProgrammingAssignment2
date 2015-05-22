## These two functions will solve the inverse of a matrix
## If the solution has been cached, then it is retrieved
## Otherwise it will solve and store the cached value

## makeCacheMatrix returns a list with the objects to
## set, retrieve, and cache the inverse of any matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the cached inverse of a matrix
## created with makeCacheMatrix if it is available
## if not it will solve the inverse and cache it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

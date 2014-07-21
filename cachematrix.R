## These functions find and cache the Inverse of a Matrix
## using the superassignment operator

## This function, makeCacheMatrix, prepares a list of functions  
## to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function uses the set and get functions prepared by
## makeCacheMatrix in order to solve for the inverse of a matrix
## only if the matrix has changed and a new inverse must be
## calculated. It the matrix has not changed the function simply
## caches the stored value of the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

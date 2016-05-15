## makeCacheMatrix
## Converts a matrix into an object that can cache the
## inverse of the passed matrix. The returned object
## can be passed to `cacheSolve`

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(m_new) {
        mat <<- new_mat
        inv <<- NULL
    }
    get <- function() mat
    set_inv <- function(new_inv) inv <<- new_inv
    get_inv <- function() inv
    
    list(set = set,
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve
## Operates on an object returned by `makeCacheMatrix`
## Either computes, saves, and returns the inverse of
## the matrix, or reads the inverse from the cache
## and returns it, saving time on consecutive calulations

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$set_inv(i)
    i
}

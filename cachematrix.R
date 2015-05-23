## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL                         ## Placeholder for inverse
    set <- function(y) {                  ## Set matrix function
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x11                 ## Return matrix function
    setinv <- function(inv) m_inv <<- inv ## Save inverse function
    getinv <- function() m_inv            ## Return inverse function
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns inverse of a matrix object from makeCacheMatrix

cacheSolve <- function(x, ...) {
    m_inv <- x$getinv()                   ## Check cache
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)                       ## Return inverse
    }
    data <- x$get()                       ## Read matrix
    m_inv <- solve(data)                  ## Compute inverse
    x$setinv(m_inv)                       ## Save inverse
    m_inv                                 ## Return a matrix that is the inverse of 'x'
}
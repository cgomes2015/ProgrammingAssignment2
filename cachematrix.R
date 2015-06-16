makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        set_inv <- function(solve) m <<- solve
        get_inv <- function() m
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

cacheSolve <- function(x, ...) {
        m <- x$get_inv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set_inv(m)
        
        return(m)
}

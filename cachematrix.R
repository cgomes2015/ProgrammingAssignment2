makeCacheMatrix <- function(x = matrix()) {
        ## x: matrix to be inversed
        ## m: object to store the inversed matrix
        ## set: function to set a matrix to 'x' and empty 'm'
        ## get: function to call the matrix stored at 'x'
        ## set_inv: function to inverse matrix 'x' (using function solve()) and store the result in 'm'
        ## get_inv: function to call the inversed matrix 'm'
        ## list: the result of makeCacheMatrix function: a list containing the above functions, to be used as input for cacheSolve()
        
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
        ## m: object where the inversed matrix cached using makeCacheMatrix() is stored (calling get_inv())
        
        m <- x$get_inv()
        
        ## if condition: check if above 'm' is null (meaning there is no previously cached Matrix).
        ## If not NULL, return cached inversed matrix 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If m=NULL, the function will pull the Matrix 'x' using the get() function, invert it through solve() and store it in 'm'
        data <- x$get()
        m <- solve(data)
        x$set_inv(m)
        
        return(m) ## it will then return 'm'
}

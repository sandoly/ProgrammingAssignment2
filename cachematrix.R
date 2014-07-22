## These two (makeCacheMatrix and cacheSolve) set of functions cache the inverse
## of a square matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## The makeVector function creates a special "matrix", which is really a list containing a function to:
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    i   <- NULL
    set <-  function(y) {
            x   <<- y
            i   <<- NULL
    }
    get <-  function()  x
    setInverse  <-  function(inverse)   i   <<- inverse 
    getInverse  <-  function()  i
    list(set = set, get = get,
        setInverse  =   setInverse,
        getInverse  =   getInverse        
        )
}

## The cacheSolve function calculates the inverse of the special "matrix" created with makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated via the getInverse function.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data (the "special" matrix) and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data    <-  x$get()
        i   <-  solve(data,...)
        x$setInverse(i)
        i        
}

## makeCacheMatrix() creates special "matrix" with a list containing several functions
makeCacheMatrix <- function(x = matrix()) {
        ## `im`` is the inverse matrix, either the calculated and cached value, or set to default NULL for new object
        im <- NULL
        ## set() sets the values of a matrix      
         set <- function(y) {
                x <<- y
                im <<- NULL
        }  
         ## get() gets the values of a matrix
         get <- function() x
         ## setinverse() sets the values of the inverse of a matrix
          setinverse <- function(solve) im <<- solve
          ## getinverse() gets the values of the inverse of the matrix
          getinverse <- function () im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() function calculates and caches the inverse matrix. 
## Argument x requires an object created with makeCacheMatrix(). 
cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        ## First it checks whether inverse value (im) has already been calculated and cached
        ## if so, it skips the calculation and returns the inverse matrix (im) and the message "getting cached data"
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        ## if im value is NULL, it calculates the inverse matrix of the data and caches it via the setinverse() function 
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
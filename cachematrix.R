## makeCacheMatrix creates an object that allows us to store a matrix and 
## retrieve it. It also allows us to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse to null on construction
        m <- NULL
        
        ## Again set the inverse to null if we assign a new matrix to the object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## Supply a list of pointers to our functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cashSolve function returns the inverse of the matrix in the 
## makeCacheMatrix object. Otherwise it calculates and stores it in the
## makeCacheMatrix before returning it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## If the inverse is already cached then return it
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        ## Otherwise, we calculate the inverse
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
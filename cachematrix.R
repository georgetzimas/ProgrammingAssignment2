## Cache the inverse of a matrix


## Write a short comment describing this function
## The first function creates a special matrix  which is a list function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse
# 4. get the inverse
# List is used an the input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                # <<- is used to assign a value to an object in an environment 
                # that is different from the current environment
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function () m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

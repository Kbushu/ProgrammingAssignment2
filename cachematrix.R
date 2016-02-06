## This function create a matrix, then everytime the inverse is required,
## it will look for the cached result first, before processing the solve function again. 
## This ensure more efficient processing.

# This part of the function serves as input for defining the matrix,
# the output, a list, make provision to contain the inverse results.
# List contains function to 
        # 1.  set the value of the matrix
        # 2.  get the value of the matrix
        # 3.  set the inverse matrix
        # 4.  get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}

# This function first look at the list to see if the inverse exist (getinverse)
# if it is not there, then the matrix is inversed, and results are set in the list, else
# it it already exist, give a message and return the cached result.

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

# A matrix
mymatrix <- makeCacheMatrix(matrix(rnorm(16),4,4))
# Do the first inverse to cache the result
cacheSolve(mymatrix)
# Now, do same and check message to indicate it comes from cache
cacheSolve(mymatrix)



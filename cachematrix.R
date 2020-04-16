## There are 2 functions. First, makeCacheMatrix creates a special "matrix" object able to cache its inverse. cacheSolve computes the inverse of the special "matrix" from above. If the inverse has already been calculated, cacheSolve will retrieve its inverse from the cache, provided the matrix has not changed.

## Within set(), m object is cleared from parent environment, and x is assigned as the input argument in the parent environment. The input argument is assigned to the value of m in the parent environment. Finally, by naming list elements we can use the $ form of the extract operator to access the functions by name.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This will population or retrieve the inverse matrix from an object of type makeCacheMatrix. If an inverse cannot be retrieved from the object passed as the argument, it is calculated here. The inverse is set in the input object an also returned to the parent environment by printing.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(m)
        n
}

#testing
A <- matrix(c(1,4,9,16),2,2)
B <- makeCacheMatrix(A)
cacheSolve(B)


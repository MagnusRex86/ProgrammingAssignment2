## Function computes the inverse of matrix with the use of caching
## First function sets the matrix, then gets the matrix
## Next the function sets the inverse of the matrix, then gets the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function()x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse=setinverse,
           getinverse=getinverse)
}


## This function solves the inverse by using the function "solve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}

## These functions cache the inverse of a matrix and will return the
## cache instead of computing it again if the matrix hasn't changed

## Function to create objects to get and set cached inverse

makeCacheMatrix <- function(x = matrix()) {
     s <-NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) s <<- inverse
     getinverse <- function() s
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## Function to return cached matrix inverser if it exists and
## matrix has not changed

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     s <- x$getinverse()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s     

}


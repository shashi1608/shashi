# shashi
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The first function, makeVector creates a special "vector", which is really a list containing a function to

# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeMatrix <- function(m = matrix()) {
    i <- NULL
  set <- function(n) {
          m <<- n
          i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(t, ...) {
  i <- t$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- t$get()
  i <- solve(data, ...)
  t$setinverse(i)
  i
}
##sample run:
## > A <- matrix(c(5,6,7,8),2,2)
## > A1 <- makeMatrix(A)
## > A1$get()
##       [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > cacheSolve(A1)
##        [,1] [,2]
## [1,]   -4    3.5
## [2,]    3    -2.5

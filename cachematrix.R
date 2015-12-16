# Caching the inverse of a matrix.
# uses solve() to calculate the inverse of a given matrix
# and by taking advantage of R's scoping rules is able to cache it's results

# Complete Example: 
# > mymat <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
# > mcm <- makeCacheMatrix(mymat)
# > cacheSolve(mcm)
# .... outputs inverse ...
# > cacheSolve(mcm)
# .... outputs inverse from cache ...

# makeCacheMatrix is a function that return a list of functions
# needed to inverse and cache a matrix
makeCacheMatrix <- function(x = matrix()) {
  # Check if we received a matrix
  if(!is.matrix(x)) {
    stop("Not a matrix")
  }
  
  # Check if we received a square matrix
  if(nrow(x)!=ncol(x)) {
    stop("Not a square matrix")
  }
  
  # xInverse holds the cached value or NULL if nothing is cached
  # initialy nothing is cached so set it to NULL
  xInverse <- NULL
  
  # setMatrix creates the matrix from input
  setMatrix <- function(y) {
    xOriginal <<- y
    # as the matrix is assigned a new value, flush xInverse
    xInverse <<- NULL
  }
  
  # getMatrix retrieves the created matrix 
  getMatrix <- function() x
  # setInverse creates and caches the inversed matrix 
  setInverse <- function(solve) xInverse <<- solve
  # getInverse retrieves the cached inversed matrix
  getInverse <- function() xInverse
  
  # return all available functions as a list
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

# cacheSolve reads cached matrix, if exsist
# if not, creates one and saves it for next time
cacheSolve <- function(x, ...) {
  # Get cached matrix
  xInverse <- x$getInverse()
  
  # if exsist cached inversed matrix, notify usage and use it
  if(!is.null(xInverse)) {
    message("Using cached data")
    return(xInverse)
  }
  
  # if not exsist inversed cached matrix, inverse matrix and cache it
  data <- x$getMatrix()
  xInverse <- solve(data)
  x$setInverse(xInverse)
  xInverse
}

# dear, dear, that was a lot of comment...
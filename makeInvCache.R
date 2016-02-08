# Inverse of a Matrix Cache:


makeInvCache <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  

# Function to compute the inverse of the matrix created by makeInvCache 

  cacheSol <- function(x, ...) {
 
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- sol(mat, ...)
    x$setInverse(inv)
    inv
  }

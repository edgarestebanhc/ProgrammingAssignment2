## Edgar Esteban Herrera Collazos for Coursera's R Programming: "Programming Assignment 2: Lexical Scoping"
## edgarestebanhc@gmail.com

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) invr <<- inverse
  get_inverse <- function() invr
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix, above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invr <- x$get_inverse()
  if(!is.null(invr)) {
    message("getting_cached_data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$set_inverse(invr)
  invr
}

###### END ######
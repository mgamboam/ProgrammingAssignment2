### cachematrix.R
## Creator: Mario Gamboa
## Date: May 18th, 2014

#' \code{makeCacheMatrix} creates a "matrix" object, with several functions associated
#' * set the value of the matrix
#' * get the value of the matrix
#' * set the value of the solved/inversed matrix
#' * get the value of the solved/inversed matrix
#' 
#' @param x the matrix to which the inverse function will be calculated for
#' @return a list of functions associated with the matrix
#' 
#' Example: `x <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))`
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


#' \code{cacheSolve} function that computes the inverse of the matrix only if the value is not in cache
#' 
#' @param x the matrix to be "inversed"
#' @return the inversed matrix
#' 
#' Example: cacheSolve(x)
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

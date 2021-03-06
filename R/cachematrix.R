### cachematrix.R
## Creator: Mario Gamboa
## Date: May 18th, 2014

#' Create matrix object with cache capabilities
#' 
#' \code{makeCacheMatrix} returns a matrix cache object based on its argument
#' 
#' @details
#' The created matrix object has several functions associated:
#' \itemize{
#'    \item set the value of the matrix
#'    \item get the value of the matrix
#'    \item set the value of the solved/inversed matrix
#'    \item get the value of the solved/inversed matrix
#' }
#'  
#' @param x the matrix definition for the object to be created
#' @export
#' @author Mario Gamboa
#' @keywords cache
#' @export
#' @examples
#'  x <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
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


#' Calculates the inverse of a matrix
#' 
#' \code{cacheSolve} computes the inverse of a matrix
#' 
#' @details
#' The method intelligently uses cached data to avoid re-calculation
#'  
#' @param x special matrix object with cache capabilities 
#' @param ... additional arguments to the regular \code{solve} function
#' @export
#' @author Mario Gamboa
#' @keywords cache
#' @export
#' @examples
#' x <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
#' cacheSolve(x)
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

## cachemean.R
## Creator: Mario Gamboa
## Reference: Taken from: https://class.coursera.org/rprog-003/human_grading/view/courses/972138/assessments/3/submissions

#' Special Vector object with cache
#' 
#' \code{makeVector} Creates a vector object with cache features
#' 
#' @details
#' creates a special "vector", which is a list containing a functions to:
#' \itemize{
#'    \item set the value of the vector
#'    \item get the value of the vector
#'    \item set the value of the mean
#'    \item get the value of the mean
#' }
#'    
#' @param x vector to which the mean function will be calculated for
#'  @export
#' @author Mario Gamboa
#' @keywords cache
#' @export
#' @examples
#' x <- makeVector(c(10,20,5,18))
#' x$get()
makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

#' Mean of Vector with cache
#' 
#' \code{cachemean} Computes the mean of a vector only if the value is not in cache
#' 
#' @details
#' The method intelligently uses cached data to avoid re-calculation
#'  
#' @param x vector to which the mean function will be calculated for
#' @param ... additional arguments to the regular \code{mean} function
#' @export
#' @author Mario Gamboa
#' @keywords cache
#' @export
#' @examples
#' x <- makeVector(c(10,20,5,18))
#' cachemean(x)
cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
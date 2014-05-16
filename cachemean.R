## cachemean.R
## Creator: Mario Gamboa
## Reference: Taken from: https://class.coursera.org/rprog-003/human_grading/view/courses/972138/assessments/3/submissions

#' \code{makeVector} creates a special "vector", which is a list containing a functions to:
#' * set the value of the vector
#' * get the value of the vector
#' * set the value of the mean
#' * get the value of the mean
#' 
#' @param x the vector to which the mean function will be calculated for
#' @return a list of functions associated with the vector
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


#' \code{cachemean} function that computes the mean of a vector only if the value is not in cache
#' 
#' @param x the vector to which the mean function will be calculated for
#' @return the mean value for the vector
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
makeVector <- function(x = numeric()) {
  nic <- NULL
  set <- function(y) {
    x <<- y
    nic <<- NULL
  }
  get <- function() x
  setmean <- function(mean) nic <<- mean
  getmean <- function() nic
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
gon <- function(x, ...) {
  nic <- x$getmean()
  if(!is.null(nic)) {
    message("getting cached data")
    return(nic)
  }
  data <- x$get()
  nic <- mean(data, ...)
  x$setmean(nic)
  nic
}


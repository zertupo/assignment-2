## first function check if the matrix already solved, save the solution if isn't the case and return the solution if it was
## the second check just in the first if for this matrix there is a save or not and return the save if it was.

## define the function relou() to assign x and la in the parent environment
## the la object clears any value that had been cached by a prior execution of cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  la<-NULL
  relou <- function(y) {
    x <<- y
    la<<-NULL
  }
  
  ## solve the matrix and save as different name in a list to use after more easily
  get <- function() x
  resolution <- function(solve) la <<- solve
  getsolve <- function() la
  list(relou = relou, get = get,
       resolution = resolution,
       getsolve = getsolve)
}


## check if there is a save for this matrix or not and return it if it was.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$resolution(m)
  m
  
  
  
  
  
}

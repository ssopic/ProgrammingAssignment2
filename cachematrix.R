##This function creates a list which contains a function to set the value of a matrix, get its value, set the value of its inverse, and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) inversematrix <<- solve
  getInverseMatrix <- function() inversematrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

#Calculates the mean of the matrix created by the function above. Before that it checks if it has already been calculated. If it is it then just uses that result. The deparse(substitute(x)) just takes the name of the input in the function and presents it in the message which could be useful for other purposes. 

cachesolve <- function(x, ...) {
  z <- deparse(substitute(x))
  inversematrix <- x$getInverseMatrix()
  if(!is.null(inversematrix)) {
    message(paste("Caching the inverse of the matrix called",z))
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setInverseMatrix(inversematrix)
  inversematrix
}

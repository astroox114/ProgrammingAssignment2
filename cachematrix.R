# Garrett Proffitt

# Creates a list containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  set <- function(y) {
      x <<- y
      matr <<- NULL
  }
  get <- function() x
  setInv <- function(inv) matr <<- inv
  getInv <- function() matr
  list(set=set,get=get,setInv=setInv, getInv=getInv)
}


# Calculates the inverse of the listed matrix created with
# makeCacheMatrix. If the inverse has been calculated, the
# function grabs the inverse from the cache and skips the
# calculation. Otherwise, the inverse is calculated, and 
# then the function sets the value of the inverse in the 
# cache via the setInv function

cacheSolve <- function(x, ...) {
  matr <- x$getInv()
  if(!is.null(matr)){
    message("getting cached data")
    return(matr)
  }
  data <- x$get()
  matr <- solve(data, ...)
  x$setInv(matr)
  matr
}

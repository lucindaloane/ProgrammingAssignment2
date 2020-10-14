#double arrow redefines an argument only in the current function environment (allows 2 levels of parameters)
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  #sets and gets value of matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #sets and gets value of inverse matrix
  setInverse <- function(inverse){
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#computing matrix - 'solves' for the inverse
cacheSolve <- function(x,...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv
}

#makes practice matrix
prac <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
prac$get()
#Tries to get inverse of matrix (returns null if nothing has been cached)
prac$getInverse()
cacheSolve(prac)

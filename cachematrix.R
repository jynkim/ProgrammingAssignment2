## As shown in the example with vectors, makeCacheMatrix function
## will be list of 4 functions that set the value of the matrix, get the value of the matrix
## and do the same for the inverse - using solve() function
## this function will take premade invertible matrix to cache the inversion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##define function to calculate matrix inversion
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## CacheSolve function utilizes list of functions defined earlier to
## cache the matrix inverse

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##retrive the matrix from x$get
  data <- x$get()
  ##run solve to get cached data
  m <- solve(data, ...)
    x$setInv(m)
    m
  
  ## Return a matrix that is the inverse of 'x'
}

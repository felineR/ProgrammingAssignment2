## This set of functions implements a caching-mechanism for inverted matrices.
## In order to save computational performance and resources these functions will 
## write a inversion calculation into a cache and retrieve it on request. 

## The 'makeCacheMatrix' function is an R function that gets a matrix as input.
## It stores the matrix's setter- and getter function, as well as the inversion of 
## the matrix with another pair of setter- and getter functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInversion <- function(inversion) {
    m <<- inversion
  }
  getInversion <- function() m
  
  list(set = set,
       get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## The function 'cacheSolve' checks whether the inversion of a matrix has
## already been calculated and stored in the cache. If it is stored, the function
## retrieves the value; if not, the value is calculated and stored in the cache.
## Returns the inversion matrix of the input matrix 'x'
cacheSolve <- function(x, ...) {
  m <- x$getInversion()
  if(!is.null(m)) {
    message("Cached data is retrieved.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInversion(m)
  m
}

## Example of user interaction
matrix1 <- matrix(c(2,2,3,2),2,2)
cacheMatrix1 <- makeCacheMatrix(matrix1)
cacheSolve(cacheMatrix1)

matrix2 <- matrix(c(1,0,5,2,1,6,3,4,0), 3,3)
cacheMatrix2 <- makeCacheMatrix(matrix2)
cacheSolve(cacheMatrix2)
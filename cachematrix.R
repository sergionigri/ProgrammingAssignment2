## makeCacheMatrix and CacheSolve are two functions that work together taking advantage of lexical scoping
## characteristic of R Programming language to save computational time and energy to invert matrixes that
## have already been calculated. Once a matrix inversion is calculated, it is stored in the computer cache memory
## for possible future usage, dispensing repetitive calculations

## makeCacheMatrix is a function that builds a set of other 4 functions within a list to the CacheSolve environment
## setmat stores the matrix you want to invert. getmat retrieves the matrix you want to invert, if previously
## requested.  setinv stores the correspondent inverted matrix. getinv retrieves the correspondent inverted matrix
## if it exists in the cache memory


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix (defined as an argument of makeCacheMatrix function)
## It checks whether this calculation has already been done.  If so, it returns the message "getting cached matrix"
## along with the correspondent inverted matrix from the cache memory, without performing new calculations.
## if not, it calculates and returns the inverted matrix as well as saves it in the cache memory 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    print ("getting cached matrix")
    return(inv)
  }
  mat <- x$getmat()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
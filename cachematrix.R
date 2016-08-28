## These two functions can be used to invert a square matrix 
## The first function "prepares" the matrix for the second function which does
## the actual inversion. If no new data is passed to the object (a list actually)
## used as an argument for the second function, 
## the result of the inversion is not calculated but retrieved from cache

##
## How to use:
## Pass the matrix to be inverted as an argument to makeCacheMatrix()
## e.g.
## m <- matrix(1:4, 2, 2)
## n <- makeCacheMatrix(m)
## 
## Pass the result of makeCacheMatrix() into cacheSolve:
## cacheSolve(n)
##

## makeCacheMatrix() creates a list containing "setters and getters" for the matrix to be inverted
## It also stores the matrix to be inverted

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve takes a list previously prepared by the makeCacheMatrix function 
## and as a result inverts the matrix passed as an argument to makeCacheMatrix
## if you pass the same list to cacheSolve (not afected by another makeCacheMatrix 
## run), it will return a previously cached result.
## You can only pass the result of makeCacheMatrix() to cacheSolve() or it will
## produce an error - cacheSolve doesn't take a matrix as an argument.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      s <- x$getsolve()
      if (!is.null(s)) {
            message("Getting cached data")
            return(s)
            }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}

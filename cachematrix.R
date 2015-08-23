## Put comments here that give an overall description of what your
## functions do
##
################################################
#
# Overall: Implements Programming Assignment 2 as described in README.md
#    Blindly copying and pasting from provided makeVector()
#    and cachemean().
#
#    Just cosmetically substituting inverse and solve for m and mean
#    but confirming it works as claimed by providing a test.
#
#    To see what it does, ource this file and run:
#       test()
#
#     https://class.coursera.org/rprog-031/
# 
# Illustrates the unintelligibility of R
#
# Shows why one should use Scipy instead of R
# and only call R packages using rpy2 when essential.
# For a more readable approach in python see:
#  http://blog2samus.tumblr.com/post/72612884464/on-decorators-closures-and-memoization-in-python
#
# Bizarrely we first create an essentially generic "makeCache..."
# function that wraps its matrix argument into a list of four functions.
# (But using specific matrix and inverse specific names instead of treating it as generic)
# 
# Then we can use the poorly named CacheSolve function to specifically
# cache an inverse of the wrapped matrix in the wrapper and return
# it without calling solve() again, each time it is used.
#
##################################################################

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Wraps a matrix (assumed invertible) into a list of four functions
  ## for use by CacheSolve().
  ##
  ## The first function, "set" holds the wrapped matrix as
  ## local variable x in the closure of makeCacheMatrix
  ## The second, "get" returns it for use by "setinverse"
  ## The third, "setinverse" is called by CacheSolve() when
  ## it has to actually calculate an inverse, using solve(),  the first time
  ## it is called and caches the result as a local variable
  ## of makeCacheMatrix.
  ## The last, "getinverse" is always called first by CacheSolve.
  ## It just returns the cached inverse if it is not NULL, but
  ## first computers it with solve() and caches it with setinverse
  ## when necessary (ie only the first time)
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Adequate short comment already included below.
##
## For longer description, works with makeCacheSolve as described above.
##
## For more helpful usage see test() below.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

test <- function(nn = 4) {
  ## 1. Create and display a poorly conditioned invertible matrix
  ## of nn rows and columns
  ## 2. Wrap it using makeCacheMatrix()
  ## 3. Display its inverse using cacheSolve()
  ## 4. Display rounded dot product that is identity matrix
  ## 5. First time message "geting cached data" will not be shown.
  ## 6. Repeat steps 3 and 4. 
  ## 7. Subsequent times will be "getting cached data".
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  ## from help(solve) Examples
  ## https://en.wikipedia.org/wiki/Hilbert_matrix
  hn <- hilbert(nn) # always poorly conditioned, has integral inverse
  print("Invertible Test Matrix (Hilbert)")
  print(hn)
  hc <- makeCacheMatrix(hn)
  hs <- cacheSolve(hc)
  print("Inverse from first call of cacheSolve() (integers)")
  print(hs)
  print("Result of Inverse * Original is Identity Matrix")
  print(round(hs %*% hn,3)) ## product of solution with original is 
  print("Again, but this time from cached previous result")
  hs2 <- cacheSolve(hc) ## using cached inverse
  print(hs2)
  print(round(hs2 %*% hn,3))
  }
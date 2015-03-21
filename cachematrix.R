## Put comments here that give an overall description of what your
## functions do

## The following functions use the potential of R to save the
## computing costs of matrix inversion by caching the inverse
## of a matrix rather than computing it repeatedly.  
## The codes written below assume that the matrix to be 
## inverted is a invertible square matrix.  
## The first function creates a special matrix object 
## that caches its inverse.  Then the second function computes
## the inverse of the special matrix returned by the first function. 
## If the inverse had already been calculated then the second 
## function retrieves the inverse from the cache.  
## In the second function the inverse is computed 
##using the solve() function.

## Write a short comment describing this function

## The first function which is the 'makeCacheMatrix', 
## creates a special "Matrix" which is a list 
## containing a function to,  
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL              # initialises the 'inverse' variable
  set <- function(y) {      # y is the arg passed to makeCacheMatrix
    x <<- y                  # set x for function environment to y
    xinv <<- NULL            # Initialises xinv to null for makevector env
  }
  get <- function() x       # creates function "get" in the makeCacheMatrix
                            # parent and assigns the input matrix 
                            # (ref cacheSolve function)
  setInv <- function(inv) xinv <<- inv # Takes value of inverse and sets to
                                       # the inverse in  makeCacheMatrix frame
  getInv <- function() xinv            # return the value of inverse from 
                                       # the makeCacheMatrix frame
                                       # (ref cacheSolve function)
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function
## Second function which is the 'cacheSolve', calculates the 
## inverse of the special matrix created with 
## the 'makeCacheMatrix' function. If the inverse has
## already been calculated then the values are retrieved
## from the cache and a message is printed. If not
## the inverse is computed and sets the value in the cache.


cacheSolve <- function(x, ...) {
                         ##  Return a matrix that is the inverse of 'x'
  m <- x$getInv()        # get the inversed matrix from 'x' environment
                         #  and assigns 'inv' value from that to this environment  
                         # it will be null if uncalculated
                         # ("xinv <- NULL" in makeCacheMatrix function)
  if(!is.null(m)) {      # if the x environment (inversion result) evaluated 
                         # then the message  and the value of inverse returned
    message("getting cached data")
    return(m)      # returns output
  }
  data <- x$get()         # if  x has not been evaluate before
                          # then get the x-vector and assign to local 
                          # variable 'data' using  x$get 
  m <- solve(data)       # compute the inverse of square matrix
  x$setInv(m)            # Assign the computed inverse to x
  m                      # return the computed result
}
      # Testing the function
NTSW <- matrix(c(2,4,3,1,5,7,4,3,8),nrow = 3,ncol = 3,byrow = TRUE)
NTSWCached <- makeCacheMatrix(NTSW)
NTSWInv <- cacheSolve(NTSWCached)
NTSW
NTSWInv


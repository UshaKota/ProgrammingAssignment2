## Put comments here that give an overall description of what your functions do
## The functions makeCacheMatrix and CacheSolve are used to compute inverse of a given matrix x
## 
## 

## Write a short comment describing this function
## makeCacheMatrix is the function object for store and retrieval of a matrix and it's inverse 
## makeCacheMatrix provides a set of helper functions to set, get the matrix and it's inverse
## makeCacheMatrix stores 2 class variables x,inv of type matrix which can be set with functions setmatrix() and getmatrix()
## x is the matrix, and inv is inverse of x
## makeCacheMatrix is instantiated as follows:
## x<-matrix(1:4,2,2),m<-makeCacheMatrix(x)
## y can be thought of as an instance to makeCacheMatrix which has a copy of x and the helper functions set,get

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the inverse to NULL
  inv <- NULL
  
  ## initialize the matrix x with the argument provided to setmatrix(y)
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## list of helper function definitions
  ## return the stored matrix x to the caller function
  getmatrix <- function() x
  
  ## set the inverse to inv to cache it's value
  setinverse <- function(inverse) inv <<- inverse
  
  ## compute the inverse if the matrix is not singular and return to the caller function
  
  getinverse <- function() inv<-solve(x)
  
  ## declare the list of helper functions
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse
  )
  
  
}


## Write a short comment describing this function
## The function cacheSolve is one of the ways of object-oriented vectorized program

## The function cacheSolve is used to compute inverse of a given matrix
## This function is extendible , takes variable number of arguments
## The first argument x is matrix
## The function checks functions as a cache for the inverse of the matrix, which means , 
## if the inverse has been computed (not null) then the function just returns that.
## This function uses a function which is makeCacheMatrix which returns a list of helper functions (function pointers) 
## for CacheSolve to store and retrieve the matrix inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## compute the inverse of the new matrix
  m <- x$getinverse()
  
  ## cache the inverse of the matrix
  x$setinverse(m)
  
  ## return the new matrix's inverse
  m
  
}

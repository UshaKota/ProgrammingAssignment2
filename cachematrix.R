## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	  inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
	  getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv<-solve(x)
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse
		 )


}


## Write a short comment describing this function
##The function cacheSolve is one of the ways of object-oriented vectorized program

## The function cacheSolve is used to compute inverse of a given matrix
## This function is extendible , takes variable number of arguments
## The first argument x is matrix
## The function 


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
	  m

}

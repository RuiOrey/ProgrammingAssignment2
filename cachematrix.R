## This file contains a pair of R functions that cache the inverse of a matrix.
## This is part of a peer assessment for the second programming assignment in the Coursera "R Programming" course.

## The function makeCacheMatrix creates a special "matrix object", which is really a list containing 
## a function to set the matrix; 
## a function to get the matrix;
## a function to set the inverse of the matrix; 
## a function to  get the inverse of the matrix.
 makeCacheMatrix <- function(x = matrix()) {
 
	 ## 0. nulls the inverse matrix 
	 m <- NULL
	 
	 ## 1. definition the function that sets the matrix
	 set <- function(y) {
		 x <<- y
		 m <<- NULL
	 }
	 
	 ## 2. definition the function that gets the matrix
	 get <- function() x
	 
	 ## 3. definition the function that sets the inverse of the matrix
	 setinverse <- function(solve) m <<- solve
	 
	 ## 4. definition the function that gets the inverse of the matrix
	 getinverse <- function() m
	 
	 ## returns the list of functions defined above
	 list(set = set, get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}


## The function cacheSolve checks to see if the inverse matrix has already been calculated. 
## If not, it  gets the inverse matrix from the cache. 
## Otherwise, it calculates the inverse matrix of the special "matrix object" created with the above function and returns it.
## The argument should be a "matrix object" of the type defined above
 cacheSolve <- function(x, ...) {

	 ## 0. gets the inverse matrix from the "matrix object" x
	 m <- x$getinverse()
	 
	 ## 1. if the inverse matrix already exists, returns it
	 if(!is.null(m)) {
		 message("getting cached data")
		 return(m)
	 }
	 
	 ## 2. otherwise, gets the matrix from the "matrix object"
	 data <- x$get()
	 
	 ## 3. calculates the inverse matrix
	 m <- solve(data, ...)
	 
	 ## 4. saves the calculated inverse matrix in the "matrix object"
	 x$setinverse(m)
	 
	 ## 5. return the inverse matrix
	 m
}

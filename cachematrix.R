####################################################################################################
####################################################################################################
####################################################################################################
#makeCacheMatrix: This function creates a special "matrix" object that cache its inverse.

#Below are two functions that are used to create a special object that stores a matrix and cache's 
#its inverse.

#The first function, makeVector creates a  "vector", which is a list containing a function to
#set the values of the matrix
#get the values of the matrix
#set the values of the inverse matrix
#get the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#The following function calculates the matrix corresponding to the inverse matrix of the x matrix
#created with the above function. However, it first checks to see if 
#the inverse matrix has already been calculated. If so, it gets the inverse  from the 
#cache and skips the computation. Otherwise, it calculates the matrix corresponding to inverse of 
#the x matrix and sets the values of the inverse matrix in the cache via the setinverse 
#function.
#Note: the function that computes inverse of a matrix is (solve)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#initiate a matrix m1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
#apply makecahematrix on the matrix to store it in a special object:
myMatrix_object <- makeCacheMatrix(m1)
myMatrix_object

#Use now cacheSolve function to get the inverse matrix of m1
cacheSolve(myMatrix_object)
#Recal again the cachesolve function for a second time->this will get directly the cached data 
#without recalculating any results! and it is well indicated in the blue message in the below console.
cacheSolve(myMatrix_object)

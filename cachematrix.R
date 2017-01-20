## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix: store a matrix and its inverse
#cacheSolve: calculate inverse of matrix and store within given makeCacheMatrix object 

## Write a short comment describing this function

#makeCacheMatrix takes a matrix as input and returns a list containing 
#functions to get/set the matrix value, and get/set the inverse matrix
#value
makeCacheMatrix <- function(matVal = matrix()) {
  #inverseVal is defaulted to NULL
  inverseVal <- NULL
  
  #set reassigns the value of matVal and returns the inverse to NULL 
  set <- function(newMat) {
    matVal <<- newMat
    inverseVal <<- NULL
  }
  
  #return matVal
  get <- function() { 
    matVal
  }
  
  #set the inverse of matVal
  #Note this function does no validation of the input
  setInverse <- function(matInverse) {
    inverseVal <<- matInverse
  }
  
  #return the assigned value of inverseVal
  getInverse <- function() {
    inverseVal
  }
  
  #return a list containing the get/set functions created above
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Write a short comment describing this function

#Given a makeCacheMatrix as input, cacheSolve will check if the
#inverse has not yet been set, and if not calculate the inverse
#of the contained matrix, and assign to the inverse matrix
cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Get the current inverse
  inverseVal <- cacheMatrix$getInverse()
  
  #If the inverse is not null, return the current inverse value
  if(!is.null(inverseVal)) {
    message("getting cached inverse data")
    return(inverseVal)
  }
  
  #Otherwise calculate the inverse, assign to the cacheMatrix,
  #and return the inverse value
  matrixVal <- cacheMatrix$get()
  inverseVal <- solve(matrixVal)
  cacheMatrix$setInverse(inverseVal)
  inverseVal
}

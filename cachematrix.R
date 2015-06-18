## These functions are responsible for performing a more optimized
## computation of matrix inversion

## This function creates a special matrix object that can cache 
## its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## Inverted X matrix
  inverseX <- NULL
  
  ## Set the matrix
  set <- function(y) 
  {
    x <<- y
    inverseX <<- NULL
  }
  
  ## Retrieve the matrix
  get <- function() 
  {
    x
  }
  
  ## Set the inverse matrix
  setInverse<- function(inverse) 
  {
    inverseX <<-inverse
  }
  
  ## Get the inverse matrix
  getInverse <- function() 
  {
    inverseX
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not already been changed), then cacheSolve should retrieve
## the inverse from the cache
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## This will either be the cached matrix or it will be null
  inverseX <- x$getInverse()
  
  ## if the inverse isn't null, then return the inverse
  if (!is.null(inverseX)) 
  {
    return(inverseX)
  } 
  else 
  {
    ## solve the inverse and store that in inverseX
    inverseX <- solve(x$get())
    
    ## set the inverse
    x$setInverse(inverseX)
    return(inverseX)
  }
}

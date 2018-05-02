## Assignment on Caching the Inverse of a Matrix using a pair of functions!

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) 
{
  ## Initialize the inverse property
  i <- NULL
  ## Method to set the matrix
  set <- function(matrix) 
  {
    m <<- matrix
    i <<- NULL
  }
  ## Method the get the matrix
  get <- function()
    ## Return the matrix
    m
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) 
  i <<- inverse
  ## Method to get the inverse of the matrix
  getinverse <- function()
  i
  ## Return a list of the methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## Just return the inverse if its already set
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  i <- solve(data)
  ## Set the inverse to the object
  x$setinverse(i)
  ## Return the matrix
  i
}

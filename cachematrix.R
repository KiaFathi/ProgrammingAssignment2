## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  ## Going to use similiar approach to example solution for vector
  ## Create list with the following methods
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse matrix
  ## 4. get the value of the inverse matrix
  inverse <<- NULL 
  set <- function(mat){
    x <<- mat
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(newinv) inverse <<- newinv
  getinverse <- function() inverse

  ## This list is the "special matrix" that we will pass to cacheSolve
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated, then 
## cacheSolve should retrieve the inverfse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Get cached value from x
  solution <- x$getinverse()

  ## If solution has already been cached for given x, return that solution
  if (!is.null(solution)){
    return(solution)
  }

  ## If no solution has been calculated, solve for solution
  solution = solve(x$get(), ...)

  ## Cache solution
  x$setinverse(solution)

  ## Return solution after caching
  return(solution)
}

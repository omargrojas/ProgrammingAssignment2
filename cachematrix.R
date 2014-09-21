## Programming assignment 2 for Coursera R Programming Course. Matrix inversion is 
## usually a costly computation and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse. It sets
## and gets the value of the function, and sets and gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ # set the value of the function
    x <<- y
    m <<- NULL
  }
  get <- function() x # get the value of the function
  set_matrix <- function(solve) m <<- solve # set the value of the matrix inverse
  get_matrix <- function() m # get the value of the matrix inverse
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  m <- x$get_matrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$set_matrix(m)
  m
}


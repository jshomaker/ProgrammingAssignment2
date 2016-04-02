## Author = J. Shomaker
## Organization = Coursera, Data Science, Programming with R, Week 3
## Assignment = Programming Assignment 2: Lexical Scoping

## Function = makeCacheMatrix, creates or gets a matrix from cache and
## sets the inverse using Solve(); assumes a square matrix of numbers

makeCacheMatrix <- function(x = matrix()) {
## x: a square matrix
  
  inv1 = NULL
  set = function(y) {

## `<<-` assigns y to a matrix in a parent environment 
    x <<- y
    inv1 <<- NULL
  }

  get = function() x
  setinverse = function(inverse) inv1 <<- inverse 
  getinverse = function() inv1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Function = cacheSolve, creates the inverse of the matrix and sets to cache

cacheSolve <- function(x, ...) {
  
  inv1 = x$getinverse()
    
## if the inverse already exists
  
  if (!is.null(inv1)){
    
## retrieve from cache and no new computation 

    message("getting cached data")
    return(inv1)

  }
  
  matrix.data = x$get()
  inv1 = solve(matrix.data, ...)
  
  x$setinverse(inv1)
  
  return(inv1)

}
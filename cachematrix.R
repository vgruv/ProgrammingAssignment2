## Function makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse.

## creates a special object that stores a matrix and cache's its inverse
## "matrix" here is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

## the <<- operator which can be used to assign a value 
## to an object in an environment that is different from the current environment. 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- matrix(nrow=0, ncol=0)
     
     set <- function(y) {
          x <<- y
          m <<- matrix(nrow=0, ncol=0)
     }
     
     get <- function() x
     
     setinverse <- function() m <<- solve(x)
     getinverse <- function() m
     
     list(set = set, 
          get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
     
}


## Function cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

## Function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     
     m <- x$getinverse()
     
     if (!all(is.na(m))) {
          message("getting cached matrix object")
          return(m)
     }
     mtx <- x$get()
     m <- solve(mtx, ...)
     x$setinverse(m)
     m
}

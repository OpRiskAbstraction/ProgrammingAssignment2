
# R-Programming Assignment 2    = Complete!

## File : cachematrix.R

## Function1: makeCacheMatrix   = Correct! Validated at 10:11pm, Sunday 2015-02-22'
## Function2: cacheSolve        = Correct! Validated at 09:15pm, Sunday 2015-02-22'

# Purpose:
## When a 'Matrix' is large, calculating the inversion of that matrix can be a costly use of Memory, 
## Instead of re-calculating the inverse of this large matrix we can take advantage of R's ability to
## cache the output. The following two functions are used to cache the inverse of a matrix.
## (Takes advantage of lexical scoping)

## Note: uses special 'superoperator': <<-

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Function 1: makeCacheMatrix
## Purpose: Creates a special 'Matrix' actually in the form of a list
## Step 1. set the value of the matrix
## Step 2. get the value of the matrix
## Step 3. set the value of inverse of the matrix
## Step 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Function 2: cacheSolve
## 1. Identifies whether cached inverse matrix result already exists
## 2. If not, then calculates the inverse matrix from the special list matrix created above
## 3. Cache calculated result for use next time

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Validation Test:

## Test Matrix
###         [,1] [,2] [,3]
###   [1,]    1    2    3
###   [2,]    0    1    4
###   [3,]    5    6    0

## Inverse of the test matrix
###         [,1] [,2] [,3]
###   [1,]  -24   18    5
###   [2,]   20  -15   -4
###   [3,]   -5    4    1

# Testing whether code handles the above 'test matrix' correctly

# Example1 : Calculates & outputs inverse matrix
  testmat <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
  testmat
  specialmat <- makeCacheMatrix(testmat)
  cacheSolve(specialmat)
#  .... outputs inverse ...

# Example 2: Draws inverse matrix from cache
  cacheSolve(specialmat)
#  .... outputs inverse from cache ...

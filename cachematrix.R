## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
  }

    ## get the value of the matrix
    get <- function() x

    ## set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    ## get the inverse of the matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve returns the inverse of the matrix.
## First it checks if the inverse has already been computed.
## If yes, it gets that value.
## If no, it computes the inverse,
## and sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ## get the inverse of the matrix        
    inv <- x$getinverse()

    ## check if the matrix exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## get the inverse of the matrix   
    data <- x$get()
    inv <- solve(data, ...)

    ## set the inverse of the matrix 
    x$setinverse(inv)
    inv
}

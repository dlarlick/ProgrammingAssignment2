## Student David Larlick
## Week 3 Assignment 2
## Creation 21Jan2016

## The following two functions are similar to the examples provided (cachemean and MakeVector)
##   The difference is that argumet for a matrix and a utilization of a solve function rather than a mean function.

## Write a short comment describing this function
## The following function accepts a matrix and stores the matrix for later retrieval from the assigned object
makeCacheMatrix <- function(x = matrix()) 
  {
    s <- NULL
    
    set <- function(y) 
    {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) s <<- inverse
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)    
    
  }

##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
##  retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  }

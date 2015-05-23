## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix requires an invertible matrix as an argument
## It assigns the passed argument to a matrix, then to function using set() function
## get() returns the matrix stored using set()
## setInverse() sets the Inverse of the matrix 
## getInverse() returns the value stored using setInverse()   
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
           get <- function() x
           setInverse <- function(solve) m <<- solve
           getInverse <- function() m
           list(set = set, get = get,
                         setInverse = setInverse,
                         getInverse = getInverse)
        
}


## Write a short comment describing this function
## cacheSolve() function takes the object created by makeCacheMatrix() as argument
## This method returns the inverse of the matrix object received through argument
## First, it checks whether the inverse has been calculated and stored 
## if it has been stored already, it returns that stored value.
## else, it calculates the inverse using the solve() funciton and stores that value
## and then returns the inverse of the matrix calculated by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}

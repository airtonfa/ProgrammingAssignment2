## This is my solution for the Programming Assignment of R.
## Airton Almeida
## Coursera, Johns Hopkins University


## Usage
## type the variable, for example MM followed by <- and the funciton, see
## the example below:
## MM <- makeCacheMatrix(matrix(1:4, 2, 2)) this should produce a matrix from
## 1 to 4 in two lines and two columns

##             [,1] [,2]
##      [1,]     1    3
##      [2,]     2    4
## to print this result, you should type: MM$get()
## to print the reverted matrix type MM$getInverse()

## The makeCacheMatrix Function is designed to cache a Matrix that will be
## inverted by the second function which is called cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
        x <<- y
        inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverted) inverse <<- inverted
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve Function gets the information produced in the makeCacheMatrix
## and produces the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse
        if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
        }
        Matrix <- x$get()
        inverse <- solve(Matrix, ...)
        x$setInverse(inverse)
}

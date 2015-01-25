## The purpose of this set of functions is to (potentially) save 
## time in computing the inverse of a matrix. For new matrices, these functions
## will manually compute the inverse of the given matrix using the solve() function.
## However, if the matrix has already been run through the functions, the cacheSolve()
## function will recognize this, and simply pull the value 


## makeCacheMatrix takes as its input an invertable (square) matrix. While
## not all square matrices are invertable, for the purposes of this function
## we assume we will not input a singular matrix.

## makeCacheMatrix has four functions inside it, set, get, setInverse, and getInverse.
## As described in the example, they set the values of the matrix and Inverse and also 
## fetch the values of the matrix and Inverse.
## A list is used to store these functions.
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inv <<- Inverse
        getInverse <- function() Inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## cacheSolve both caches and solves (calculates the Inverse) of the supplied 
## matrix. For our puposes, the if statement (if the inverse exists in getInverse)
## then find that value and return it. This is the time-saving step. To let you know
## that the function has found the inverse. Otherwise, the function will compute the 
## Inverse and cache it.
cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setInverse(Inv)
        Inv
}

## Sample input/output: 
##      matrixtotest<-matrix(rnorm(100),10,10)  A square matrix of 100 random variables
##      caching<-makeCacheMatrix(matrixtotest)
##      cacheSolve(caching)
## (returns a 10 x 10 inverse matrix)
##      cacheSolve(caching)
## (returns the same inverse with a note that it was found in cache)
## (work done for second run of cacheSolve is marginally faster on my computer)


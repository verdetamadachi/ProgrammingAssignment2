## This file contains two functions:
##      makeCacheMatrix - This function create inversed matrix cache.
##      cacheSolve - This function computes inverse of matrix given. 
##                   If inverse matrix doesn't exist in cache it caches it using function makeCacheMatrix.
##                   If inversed matrix exits it pulls it from cache "InversedMatrix" variable.
## Matrix rules - any given matrix must be square 2 X 2; 3 X 3
## If it's not square you will get an error  "Error in solve.default(data, ...) : 'a' (3 x 2) must be squar"


## Create a special "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #InvercedMatrix: matrix object to store inversed matrix 
        InversedMatrix <- NULL
        
        
        setCache <- function(y){
                x <<- y
                InversedMatrix <<- NULL
        } 
        
        getCache <- function() {x}
        
        #===== Inversed matrix set and get functions
        
        setInversedMatrix <- function(matrix) {
                InvercedMatrix <<- matrix
        }
        
        getInversedMatrix <- function() {
                InversedMatrix}
        
        
        list(setCache = setCache, getCache = getCache, setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix )
}


## If new matrix then fucntion inverses matrix using solve function
## If matrix was inversed before function returns cached inversed matrix
## To run do create squre matrix for example C = matrix( c(1, 1, 311, 111), nrow=2, ncol=2)
## And use consol to run cacheSolve cacheSolve(makeCacheMatrix(C), C)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInversedMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else{
                data <- x$getCache()
                m <- solve(data, ...)
                x$setInversedMatrix(m)
                m
        }
        
}

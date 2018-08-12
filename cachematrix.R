## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

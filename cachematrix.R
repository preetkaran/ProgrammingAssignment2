
## In funtion makeCacheMatrix, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the InverseMatrix
#get the value of the Inverse matrix
#Im is Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
        Im <- NULL
        setMatrix <- function(y) {
            x <<- y
            Im <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) Im <<- solve
        getInverse <- function() Im
        list(setMatrix = setMatrix, getMatrix = getMatrix,
                setInverse = setInverse,
                getInverse = getInverse)

}


## The function cacheSolve below calculates the Inverse  of the special "matrix" created with the above function. 
#However, it first checks to see if the Inverse Matrix has already been calculated. If so, it gets the Inverse Matrix from the cache and 
#skips the computation. Otherwise, it calculates an Inverse Matrix of the data and sets the value of the Inverse Matrix in the cache 
#via the setInverse function.
#Im is Inverse of the Matrix
cacheSolve <- function(x, ...) {
        
        Im<- x$getInverse()
        if(!is.null(Im))
        {
            print("getting cached data")
            return(Im)
        }
        
        data<-x$getMatrix()
        Im<-solve(data,...)
        x$setInverse(Im)
        Im
}

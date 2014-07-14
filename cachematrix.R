## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(im) inverseMatrix <<- im
        getinverse <- function() inverseMatrix
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)){
                message("Getting cached matrix")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setinverse(inverseMatrix)
        inverseMatrix
}

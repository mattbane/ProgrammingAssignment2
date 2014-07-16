## This is an example on how R can simulate object oriented programming.  
## makeCacheMatrix is a function which contains a list of functions.  You can 
## can create an instance of makeCacheMatrix which will represent the class and 
## and access the elements of the list which represent the methods.  R can also 
## simulate instance variables (x and inverseMatrix here) which are private and 
## can be set by the functions in the list via <<- operator.  This works because
## <<- can access variables defined in its parent environment.  

## makeCacheMatrix is a function which contains a list of four functions and an 
## argument x which is a matrix.  It also has a private variable which contains the 
## inverse of the matrix x.  set takes one argument y which reassigns the 
## matrix x and reassigns the inverse matrix, inverseMatrix.  get returns the 
## matrix.  setinverse take one argument, a matrix, and assigns it to inverseMatrix.
## getinverse returns the inverse Matrix variable inverseMatrix.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        set <- function(y){
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(im) inverseMatrix <<- im
        getinverse <- function() inverseMatrix
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

## cacheSolve takes an instance of makeCacheMatrix and returns the inverse of the 
## matrix.  First it retrieves the inverse matrix from makeCacheMatrix.  If it is 
## not null it returns the matrix instead of recalculating the inverse.  If it is
## null it get the matrix, calculates the inverse via the solve function, sets the 
## inverse in makeCacheMatrix and returns the newly calculated inverse matrix.
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

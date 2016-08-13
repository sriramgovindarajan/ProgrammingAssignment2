##Sriram Govindarajaan Assignment #2
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the special matrix created with the above function. 
#However, it first checks to see if the inverse of the matrix has already been calculated. 
#If so, it gets the matrix from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache 
#uaing the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_matrix<- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("No need to compute; getting cached inverse matrix")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}

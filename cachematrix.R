## These functions reduce the amount of computation for inverting a matrix.  

## makeCacheMatrix stores a list of functions. The set function will store a 
## matrix. The get function will retrieve the stored matrix. The setinverse
## function will set the inverse of a matrix and the getinverse will retrieve
## the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y){
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) M <<- solve
        getinverse <- function() M
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that looks for the stored inverse of a given matrix. If
## it has already been computed, it will return the answer, otherwise, it will
## compute the inverse and store it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        if(!is.null(M)){
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setinverse(M)
        M
}

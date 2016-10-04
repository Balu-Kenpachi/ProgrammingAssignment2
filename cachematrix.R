## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMat <<- inverse
        getinverse <- function() invMat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invMat <- x$getinverse()
        if(!is.null(invMat)) {
                message("getting cached data.")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data)
        x$setinverse(invMat)
        invMat
}


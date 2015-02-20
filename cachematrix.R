## Functions will cache matrix inverse so it can be reused

## This function is a vector with following functions: set matrix value, get matrix value, 
## set inverse, and get inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cachSolve will check if matrix inverse has been already cached. If so, it will returne cashed value. 
## If not, it will will calculate inverse and will cache it. 

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
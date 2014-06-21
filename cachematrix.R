##    create/instantiate a cache matrix object with 4 methods and includes an 
##      object variable, 
##          'inverse': stores inverse matrix  
##  usage:
#       a <- makeCacheMatrix(matrix( c(2, 4, 3, 5), nrow=2,ncol=2))
##  alternate usage (this uses the object's set method):
#       a <- makeCacheMatrix()
#       a$set(matrix( c(2, 4, 3, 5), nrow=2,ncol=2))
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL     # instance variable for inverse matrix
    Set <- function(y) {  #
        x   <<- y
        inverse <<- NULL
    }
    Get <- function()
        x
    SetInverse <- function(im) 
        inverse <<- im
    GetInverse <- function()
        inverse
    
    list(set = Set,
            get = Get,
            setinverse = SetInverse,
            getinverse = GetInverse)

}



## Return a matrix that is the inverse of 'x'
## returns cached inverse if available, other calculates and cache inverse
## usage
#       cacheSolve(a)
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(is.null(m)){       # no inverse cached
        data <- x$get()
        m <- solve(data)     # calcuate  inverse
        x$setinverse(m)   # cache inverse
    } else {
        message('using cache vakye')
    }
    m
}


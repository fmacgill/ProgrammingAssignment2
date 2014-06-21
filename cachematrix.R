##    create/instantiate a cache matrix object with 4 methods and includes an 
##      object variable, 
##          'inverse': stores inverse matrix  
##  usage:
#       a <- makeVector(1:11)
##  alternate usage (this uses the object's set method):
#       a <- makeVector()
#       a$set(1:11)
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
    
    list = (set = Set,
            get = Get,
            setinverse = SetInverse,
            getinverse = GetInverse)

}



## Return a matrix that is the inverse of 'x'
## returns cached inverse if available, other calculates and cache inverse
## usage
#       cacheSolve(a)
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(is.null(m)){       # no inverse cached
        data <- x$get()
        m <- solve(x)     # calcuate  inverse
        x$setInverse(m)   # cache inverse
    } else {
        message('using cache vakye')
    }
    m
}


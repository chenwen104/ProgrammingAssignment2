## construc a special matrix and cache its inverse

## construct a special matrix
makeCacheMatrix <- function(x = matrix()) {
    ##set the value of the matrix
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ##get the value of the matrix
    get <- function() 
    
    ##set the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    
    ##get the value of the inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ##check whether inverse has been stored
    if(!is.null(inv)){
        message("getting cached inverse data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}



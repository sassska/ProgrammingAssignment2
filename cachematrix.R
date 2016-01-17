## The following two functions cache the inverse of a matrix.


## The function makeCacheMatrix() creates a special "matrix" object that can
## cahe its inverse. It does the following:
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the elements of the inverse matrix
### 4. get the elements of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get =get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The function cacheSolve() computes the inverse of the special "matrix" retur ## ned by makeCacheMatrix above. If the inverse has already been calculated (and## the matrix has not changed), then the cachesolve should retrieve the inverse ## from the cache. Otherwise it calculates the inverse of the data and sets the ## value of the inverse matrix in the cache via the setinverse function. The inv## erse matrix is returned in either case.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

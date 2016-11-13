## funnctions makeCacheMatrix and cacheSolve used togther can avoid costly inverse calculations of matrices. makeCacheMatrix creates
## a special matrix that can cache it's inverse. cacheSolve checks to see if there is a cached version created by makeCacheMatrix
#####################
## To test, use the following:
##  A = matrix( c(2, 4, 3, 1),nrow=2,ncol=2, byrow = TRUE)
##  A
#### Result should be:
## [,1] [,2]
## [1,]    2    4
## [2,]    3    1
##  cachedMatrix <- makeCacheMatrix(A)
##  cacheSolve(cachedMatrix)
#### Result should be:
## cacheSolve(cachedMatrix)
## [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2
## Now run this again: cacheSolve(cachedMatrix)
#### Result should be:
## Yo! getting cached data
# [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2


###########################################################

## makeCacheMatrix creates a special matrix that can cache an inverse.  It returns a list of functions which:
## 1. (set) set the value of a matrix
## 2. (get) get the value of a matrix
## 3. (setinv) set the value of matrix inverse
## 4. (getinv) get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve calculates the inverse of a matrix returned by makeCacheMatrix.  It will check to see if the matrix is cached first.  
## If it is it will return the cached data

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
        message("Yo! getting cached data")
        return(inv)
    }
    
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    
    return(inv)
}

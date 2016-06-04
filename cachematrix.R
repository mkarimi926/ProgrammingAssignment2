## These functions provide a cached version of Inverse of a matrix.
## With this module you can avoid time consuming computation on inverse of matrix, you just compute the inverse and use it 
## every where without any more computation.
## In order to use first call XC=makeCacheMatrix(X) to provide the special list containing required information, and then call cacheSolve(XC) 
## to use the inverse of the X matrix every where you want without extra computation.

## Created by mKarimi926 on 4 June 2016

## This function provide a special list contatining required information about the matrix X and its cached inverse(x_1).

makeCacheMatrix <- function(x = matrix()) {
        x_1 <- NULL                             ## x_1 is the inverse of x
        set <- function(y) {                    ## sets the value of x in cached matrix list
                x <<- y
                x_1 <<- NULL                    ## if the value of matrix x is change then the inverse should be recalculated
        }
        get <- function() x                     ## get the matrix x from cached matrix list
        setinverse <- function(solve) x_1 <<- solve     ## compute the inverse of input matrix and put the result on x_1
        getinverse <- function() x_1            ## get the cached inverse matrix value which is stored in x_1
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## return the result list of functions defined.
}


## This function provides the cached version of inverse of a matrix based on the result matrix X prepared by makeCacheMatrix
## If the matrix X is changed it will compute the inverse again. If the matrix X is not changed it will return the cached inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_1 <- x$getinverse()                   ## get the inverse from cahced environment
        if(!is.null(x_1)) {                     ## if the inverse is not null then return that value
                message("getting cached data")
                return(x_1)
        }
        data <- x$get()                         ## the inverse does not exist so we should compute it. So first we get data
        x_1 <- solve(data, ...)                 ## then compute inverse and
        x$setinverse(x_1)                       ## put it in cached list to be used later
        x_1
}
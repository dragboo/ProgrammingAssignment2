## Put comments here that give an overall description of what your
## functions do
## There are two functions makeCacheMatrix and cachesolve. The makeCachematrix crreates 
## matrix object which contain functions. The cachesolve computes the inverse if it is not present
## If it is present then it returns the inverse of the matrix.


## Write a short comment describing this function
## This function creates a matrix object which contains the functions:
## set(), get(), setinverse(), getInverse()
## it takes a matrix as argument.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getInverse=getInverse)
}


## Write a short comment describing this function
## This function computes inverse of the "matrix" object from the previous function
## it is stored in m and returned
## It takes the matrix object as argument.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

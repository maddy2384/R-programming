## Put comments here that give an overall description of what your
## functions do

## There are two functions, the first one, makeCacheMatrix is used to create a special matrix, which contains
## both the matrix and the inverse of it.
## The second one, cachesolve is a function to return the inverse if the inverse if already cached, while if
## not, cachesolve will calculate the inverse and store it into makeCacheMatrix

## Write a short comment describing this function

## makeCacheMatrix returns a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) in <<- inverse
        getinverse <- function() in
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}



## Write a short comment describing this function

## cachesolve calculates the inverse of the special "matrix" created by the above function. If the inverse has
## been calculated already, it will directly return the inverse. Otherwise, this function will use solve to
## calculate the inverse of the matrix and update the cache of the inverse matrix in the list.

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in <- x$getinverse()
        if(!is.null(in)) {
            message("getting cached data")
            return(in)
        }
        matrix <- x$get()
        in <- solve(matrix, ...)
        x$setinverse(in)
        in
}

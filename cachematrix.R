## Put comments here that give an overall description of what your
a## functions do

## The following makeCacheMatrix function takes a matrix and creates a list with the following functions
## get -> gets the value of the matrix
## set -> sets the value of the matrix
## get_inverse -> gets the inverse value of the matrix
## set_inverse -> sets the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The following cacheSolve function takes the list created by the makeCacheMatrix function as input
## and checks if the inverse is present in the cache. If so, return from cache else calculate the inverse,
## save in the cache and return the inverse

cacheSolve <- function(x, ...) {
        
        m <- x$get_inverse()
        #check if the inverse if already calculated
        if(!is.null(m)) {
                message("getting cached data")
                #return data from cache
                return(m)
        }
        #else get the matrix and calculate the inverse and set the value in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        # return inverse
        m
}
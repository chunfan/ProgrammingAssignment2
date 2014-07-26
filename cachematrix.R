## Matrix inversion is usually a costly computation. 
## These pair of functions perform matrix inversion 
## and cache the inverse for subsequent use, if there 
## is no change to original matrix. 

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInv(inv)
    inv
}

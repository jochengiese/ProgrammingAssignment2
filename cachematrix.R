## This is my solution to homework assignment 2 in "R
## programming". These two functions allow to use an extension of a
## matrix with its own inverse, If accessed multiple time, the inverse
## of the matrix does not need to be recomputed.

## initiailizes the generalized matrix object together with some computation methods
 makeCacheMatrix <- function(x = matrix()) {
 invMatrix <- NULL;
 set <- function(a) {
     x <<- a;
     invMatrix <<- NULL
 }
 get <- function() x
 setInverse <- function(anInv) invMatrix <<- anInv
 getInverse <- function() invMatrix
 list(set = set, get=get, setInverse = setInverse, getInverse=getInverse)
 }


## This is the extension of the regular "solve" method for the cached
## matrices Return a matrix that is the inverse of 'x'. If x has value
## cached, return it, otherwise compute and store it.
cacheSolve <- function(x, ...) {
   
    anInv <- x$getInverse()
    if(!is.null(anInv)) {
 #       message("getting cached data")
        return(anInv)
    }
    # otherwise anInv is null and we need to compute it
    data <- x$get()
  #  message("computing inverse");
    anInv <- solve(data,...)
    # now set this inverse in the x object
    x$setInverse(anInv)
    # make sure the inverse is returned to calling function
    anInv
}

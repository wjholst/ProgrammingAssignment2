# The following functions provide a means to cache the results of a matrix inversion.  
# The purpose of this exercise is to take advantage of R properties to create essentially
# a static variable which contains the inverse of an existing matrix to avoid computational
# cost of inverting the matrix each time it is needed.
#
# The function is somewhat brittle.  It does not test to ensure that the input matrix was
# square (nxn) or whether it is invertable, so it is subject to potential run-time errors
#
# Calling information:
#  
#   m <- matrix (rnorm(25,5,5))         # build an initial 5x5 test matrix
#   mCached <- makeCachedMatrix(m)      # creates the special cached version of the matrix
#                                         and the getter/setter functions
#   mInverse <- cacheSolve(m)           # First call both creates and returns the inverse
#                                         and creates the cached, static copy of the inverse
#   mInverse2 <- cacheSolve(m)          # Subsequent call returns the cached copy; it does
#                                         have to re-invert the matrix

#
# makeCacheMatrix establishes the getter and setter functions for the matrix and the inverse
#
makeCacheMatrix <- function (x=matrix()) {
    inv <- NULL
    #  set the initial cached matrix and inverse
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    #  get the static value of the matrix
    get <- function () x
    
    #  set the static value for the matrix inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get the static value of the matrix inverse
    getinv <- function () inv
    
    # establish the list of return objects
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    
}

#
#  cacheSolve returns the inversion of the special matrix managed by our makeCacheMatrix functions
#
cacheSolve <- function(x, ...) {
 
    inv <- x$getinv()  # attempt to get our cached inverted matrix
    # if the matrix has been cached, return the cached version
    if (!is.null(inv)) {
        message("getting cached matrix")
        return (inv)
    }
    # otherwise it is necessary to used the setinv to create the cached version
    data <- x$get()
    # use the solve function to invert
    inv <- solve(data, ...)
    # save the cached version
    x$setinv (inv)
    inv
}
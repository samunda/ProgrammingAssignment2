## This R script provides two functions (makeCacheMatrix and cacheSolve) which
## permit computing an inverse of a matrix. The key thing is repeated calls to
## cacheSolve (which does the inversion) with the same input matrix does not
## involve recomputation of the inverse.
##

## Construct a cacheable matrix object (which can store the input and inverse).
##
##  Usage 1:    M_cached = makeCacheMatrix( matrix(1:4, 2, 2) )
##
##  Usage 2:    M_cached = makeCacheMatrix()
##              M_cached$set( matrix(1:4, 2, 2) )

makeCacheMatrix <- function(x = matrix())
{
    # local variable to store the inverse.
    inv <- NULL

    # Store (i.e. set) input matrix.
    set <- function(y)
    {
        # assign new input matrix y to the function argument x.
        x <<- y

        # reset the inverse to null.
        inv <<- NULL
    }

    # Get the matrix stored internally.
    get <- function()
    {
        x
    }

    # Store inverse.
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }

    # Get the inverse stored internally.
    getInverse <- function()
    {
        inv
    }

    # return setter and getter functions.
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## Compute the inverse of a cacheable matrix object (made using makeCacheMatrix)
##
##  Usage:  cacheSolve( M_cached )

cacheSolve <- function(x, ...)
{
    # What is the currently stored inverse?
    val <- x$getInverse()

    # Is the currently stored inverse valid? (Note:if not null then it is valid)
    if(!is.null(val))
    {
        message("getting cached data")

        # return the inverse.
        return(val)
    }
    # else perform below computations.

    # What is the input matrix?
    data <- x$get()

    # calculate the inverse of the input matrix.
    # (pass along provided arguments as well)
    val <- solve(data, ...)

    # store (cache) the computed inverse.
    x$setInverse(val)

    # return the inverse.
    val
}
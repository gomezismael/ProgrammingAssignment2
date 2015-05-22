## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          ## Create "matrix" object (list of functions) from matrix

    # Initialization for first time the "matrix" is created
    Inv = NULL

    # Setter for when we reassign the original matrix to the "matrix"
    set = function(Y) {
        # new matrix to consider
        x <<- Y
        # reinitialization for the inverted matrix cache
        Inv <<- NULL
    }

    # give back the matrix
    get = function() x

    # set the cached value (will be used after calculation)
    setInv = function(InvMat) Inv <<- InvMat

    # get the cached value
    getInv = function() Inv

    # return "matrix" which is a list of functions w/ closure
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # get the current cached value
    Inv = x$getInv()

    # check the cached value
    if(!is.null(Inv)){
        # return the cache value, as it is meaningfull
        message("getting cached data")
        return(Inv)
    }

    # otherwise (cached value not yet calculated)
        # get matrix from closure of the "matrix" functions
    data = x$get()
        # calculate inverted matrix
    Inv = solve(data, ...)
        # set it as the cached value in the "matrix"
    x$setInv(Inv)
        # return the calculated (and cached) value
    Inv
}

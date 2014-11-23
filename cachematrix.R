## makeCacheMatrix and cacheSolve prevent a matrix inverse from being recalculated if it has already been determined.
## We assume the matrix is invertible. 

## makeCacheMatrix - a function that takes an invertible matrix x and initialises the inverse variable n to NULL. 
## Local functions get, set, getinv and setinv are for getting and setting x and getting and setting its inverse n.
## Returns a list containing the internal functions set, get, setinv and getinv, making these functions 
## accessible outside of makeCacheMatrix.
## Note: Although all internal functions are accessible, setinv should only be called by cacheSolve as an incorrect 
## non-NULL value will prevent the inverse from being recalculated.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinv <- function(inv) n <<- inv
        getinv <- function() n
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve - here variable x is the function list returned by makeCacheMatrix.
## If x already has an inverse then return it - this is our 'cached' value, otherwise calculate the inverse 
## using the solve function and the get and setinv functions defined in makeCacheMatrix, 
## then return the newly calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinv()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinv(n)
        n
}



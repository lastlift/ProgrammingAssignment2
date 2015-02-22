## ----------------------------------------------------------
## makeCacheMatrix -
##
##   defines a function that takes a matrix as a parameter, 
##   and defines access methods to the matrix and a 
##   value of the matrix inverse, to be calculated and stored

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() {
                x
        }
        setinv <- function(inv) {
                m <<- inv
        }

        getinv <- function(){
                m
        }

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## ----------------------------------------------------------


## ----------------------------------------------------------
## cacheSolve -
##
##   defines a function that takes an object defined by a
##   makeCacheMatrix function.  computes the inverse of the 
##   object's stored matrix value
##
##   assumes the matrix is square and invertible

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        
        if(!is.null(m)) {
                return(m)
        }
        
        data <- x$get()

        m <- solve(data, ...)
        
        x$setinv(m)
        
        m
}
## ----------------------------------------------------------


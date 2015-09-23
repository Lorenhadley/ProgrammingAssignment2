##  This set of functions will cache the inverse of a solveable matrix
##  Initialize the process by creating a Matrix myVariable <- makeCacheMatrix(x) where X
##  is a solveable matrix. 
##
##  Calling the cacheSolve(myVariable) will calculate the inverse of the
##  matrix is the inverse hasn't been solved, or it will access the value from the cache 
##  if the inverse has been calculated and stored, saving on processing load.
##
##  myVariable$get() will return the value of the original matrix
##  myVariable$getinverse() will return the inverse
##  myVariable$setinverse is used internally to set the inverse and should not be manually called.


## This function creates a special "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
          }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

        }


##  This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        ## Return a matrix that is the inverse of 'x'
        }

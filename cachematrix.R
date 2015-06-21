## The two functions in this file are used to create a special object that stores a numeric matrix and caches 
## its inverse.
## makeCacheMatrix and cacheSolve are appropriate to use when an application requires a matrix that 
## changes infrequently, but whose inverse needs to be accessed repeatedly.  Appropriate use of 
## makeCacheMatrix and cacheSolve guarantees that the inverse of the given matrix is computed at 
## most once (unless the matrix is later modified.) 

## makeCacheMatrix creates a special matrix, which is actually a list containing four functions:
##	1. set()  		- sets the value of the matrix
##	2. get() 		- retrieves the value of the matrix
##	3. setInverse() 	- sets the value of the inverse
##	4. getInverse()	- retrieves the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL	         

         set <- function(y) {    ## changes the value of the matrix
                 x <<- y	        ## store the new matrix
                 inv <<- NULL        ## indicate that the inverse of the new matrix is not yet computed
        }

        get <- function() { x }     			## returns the stored matrix

        setInverse <- function(inverse) {
	inv <<- inverse			## cache the newly computed inverse
        } 

        getInverse <- function(){ inv }  		## retrieve the cached inverse

        list(set = set,				## return a list of the accessor functions
                get = get,                                                                        ## for the cached matrix
                setInverse = setInverse,
                getInverse = getInverse)
}


## cacheSolve returns the inverse of the special "matrix" created with the above function.  However,
## it first checks to see if the inverse has already been computed.  If so, it retrieves the inverse from 
## cache and skips the computation.  Otherwise, it computes the inverse and sets the value of the 
## inverse in cache via the setInverse function.
## Note that this code assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()              ## use an accessor function to retrieve the cached inverse of the given matrix
        if (!is.null(inv)){		## check whether the inverse has been computed yet, and if so return it
                message("getting cached data")
                return(inv)
        }     
        ## The following code is executed only if the inverse has not yet been computed and cached
        m <- x$get()              	## use an accessor function to get the stored matrix 
        inv <- solve(m)		## compute the inverse of the stored matrix
        x$setInverse(inv)	## use an accessor function to cache the computed inverse
        inv			## finally, return the inverse to the calling function
}

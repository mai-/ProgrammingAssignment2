## these 2 functions listed below take a matrix and cache its inverse
## for future use, so that when you request the inverse of the same
## matrix again, it would return its cached value in lesser time

## makeCacheMatrix, takes a matrix and returns a list of 4 items 
## containing results from 4 functions that are included in this 
## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL   			## i which is the inverse of the matrix
								## is first initialized to NULL
        
		set <- function(y) {	## set x to y's value
                x <<- y			## we use the <<- operator as x isn't found
                i <<- NULL		## in the set function's local environment
        }
        get <- function() x		## get the stored value of x
        setinv <- function(inv) i <<- inv	## similar to set function
											## but this time we set i to
											## the inverse of the matrix
        
		getinv <- function() i				## similar to get, but returns i's value
        
		list(set = set, get = get,			## the returned argument is a list
             setinv = setinv,				## that contains the values of the
             getinv = getinv)				## results of all 4 functions


}


## cacheSolve, this function computes the inverse of the matrix 
## if its not already computed, and returns the cached inverse of the
## matrix if the computation has been done previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()			## get the value of the inverse 
        if(!is.null(i)) {		## if cache is found
                message("getting cached data")	##type this message
                return(i)		## and return the cached value
        }
        data <- x$get()			## if cache not found
        i <- solve(data, ...)	## compute it here
        x$setinv(i)				## save it to cached values
        i						## return the inverse as a result


}

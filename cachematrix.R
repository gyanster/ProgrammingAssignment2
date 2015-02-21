## makeCacheMatrix creates a special matrix, cacheSolve computes the inverse of the special "matrix"

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##creating variable M to store the inverse of the created matrix
		m <- NULL
		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		##defining the get function
        get <- function() x
		
		##defining the setmatrix function
        setmatrix <- function(solve) m <<- solve
		
		##defining the getmatrix function
        getmatrix <- function() m
		
		##generating the list of functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		
		##running the getmatrix function and assigning it to m
		m <- x$getmatrix()
        
		##checking if m is already in the cache and returning its value
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		##get function logic
        matrix<-x$get()
		
		##calculating the inverse of the given matrix and assigning it to m
        m <- solve(matrix,...)
		
		##setmatrix function logic
        x$setmatrix(m)
		
		##returning m
        m
}

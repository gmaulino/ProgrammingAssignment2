## Caching the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
        ## assign NULL for default matrix
        s <- NULL
        
        ## set matrix and reset its inverse
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
   		
   		## retrieve matrix
        get <- function() x
        
        ## cached inverse
        setsolve <- function(solve) s <<- solve
        
        ## retrieve cached inverse matrix
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 

}

## This function computes the inverse of the matrix returned by makeCacheMatrix above: 
## if the inverse has already been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	
		## retrieve cached inverse matrix
        s <- x$getsolve()
        
        ## if it has been calculated
        if(!is.null(s)) {
                
                ## return cached inverse
                message("getting cached data")
                return(s)
        }
        
        ## retrieve original matrix
        data <- x$get()
        
        ## calculate inverse matrix
        s <- solve(data, ...)
        
        ## cache inverse matrix
        x$setsolve(s)
        
        ## Return inverse matrix
        s       
}

## Functions that compute and store in cache the inverse of a matrix

## This function puts the inverse of a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
	
	##Initialization of the variable that will hold the inverse matrix
	inverseMatrix<- NULL 

	    
	## Definition of the set function to assign a new matrix
    	set <- function( newMatrix ) {
            x <<- newMatrix 
            inverseMatrix <<- NULL
    	}

    	## Definition of the get function to retrieve a matrix

    	get <- function() {
    	
	## Return the matrix
    	x
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inverseMatrix
    }

    ## Return a list of the available methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)



}


## This function computes the inverse of a matrix if necessary (not in the cache)
## if the inverse is stored in the cache so it is retrieved;

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMatrix <-x.$getinverse()
     
	## Test if the matrix is already stored in the cache
	if(!is.null(inverseMatrix )) {
         message("The inverse Matrix is already in the cache")
         return(inverseMatrix )
     }
	## Retrieve the Matrix 
	standardMatrix <- x$get()
     
	## Compute the inverse Matrix using matrix multiplication 
	inverseMatrix <- solve(standardMatrix, ...) %*% standardMatrix
     
	## Set the value 
	x$setinverse(inverseMatrix )
     inverseMatrix 


}

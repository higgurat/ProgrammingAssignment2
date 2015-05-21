## These functions cache the inverse of a matrix

## Creates a special object that can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    #setter function for the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #getter function for the matrix
    get <- function() x
    #setter function for the inverse matrix
    setInverse <- function(inverse) i <<- inverse
    #getter function for the inverse matrix
    getInverse <- function() i
    
    #return list of getter/setter functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of a matrix, by first checking against cache 
## if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i)) { #check if the inverse matrix is already cached
        message("getting cached data")
        return(i) #return cached value
    }
    data <- x$get()
    i <- solve(data, ...) #calculate the inverse matrix
    x$setInverse(i) #update the cache
    i #return inverse matrix
}


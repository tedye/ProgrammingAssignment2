## The functions below are to cache the inverse of a matrix.

## This function creates a special "matrix" object that can 
## cache its inverse in mat.
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){
                x <<- y
                mat <<- NULL
        }
        get<-function() x
        setInverse <- function(inverse) mat <<- inverse
        getInverse <- function() mat
       
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        mat <- x&getInverse()
        if (!is.null(mat)){
                message("getting cached inverse of matrix")
                return(mat)
        } 
        data <- x$get()
        mat <- solve(data, ...)
        x$setInverse(mat)
        mat
        ## Return a matrix "mat" that is the inverse of 'x'
}

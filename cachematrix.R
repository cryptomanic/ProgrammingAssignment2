# makeCacheMatrix returns a list of function
# set -> to set the matrix
# get -> to get the matrix
# setInverse -> to set inverse of matrix
# getInverse -> to get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse){
      inv <<- inverse
    }  
    getInverse <- function() inv
    list(set = set,get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve function first check whether
# the inverse is already calculated or not.
# If it is already calculated then simply
# returns it otherwise find the inverse 
# and then returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}

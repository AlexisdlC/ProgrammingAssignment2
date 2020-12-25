## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      ## Function to set-up cache matrix inverse
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x 
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      ## Function looks at cache and returns inverse in cache if it exists otherwise,
      ## it computes the inverse and saves it in the cache
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special matrix that I will use later to cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## restore a cache inverse if it exists 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x&getinv()
      if(!is.null(i)) {
            message('getting cache inverse')
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}

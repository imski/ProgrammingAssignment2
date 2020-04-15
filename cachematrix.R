## Set of funcions to cache a matrix and its inverse

## makeCacheMatrix will create a list of functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix (solved matrix)
## 4. get the inverse matrix (solved matrix)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolved <- function(solve) m <<- solve
      getsolved <- function() m
      list(set = set, get = get,
           setsolved = setsolved,
           getsolved = getsolved)
}

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolved()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolved(m)
      m      
}

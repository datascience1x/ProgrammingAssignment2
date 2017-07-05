## this function is used to calculate the inverse of matrix and 
## store it to the cache so that the value can be called rather
## than having to re-calculate the inverse of the matrix 

## the makeCashMatrix function is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## this definites the cache as "m"
    m <- NULL 
  set <- function(y) {
    x <<- y ## assigns matrix input to y
    m <<- NULL ## re-sets m to null
  }
  get <- function() x ## retrieves the matrix x
  setinverse <- function(inverse) m <<- inverse ## the inverse of the 
  ## matrix x is equal to m which is set in the cache 
  getinverse <- function() m ## retrieves the cache value for the invese
  # matrix x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getsetinverse)
}


## the function below calculates the inverse of a special matrix
## created with the makeCacheMatrix function above. However, it first
## checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cash and skips computation. Otherwise,
## it calculates the inverse of the data in the matrix and sets the value
## of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {

          ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}

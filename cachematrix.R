## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix creates a matrix object that can cache its inverse
##cacheSolve function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix first sets the inverse of x to NULL and then proceeds by setting the data to a new matrix y
## it then sets a get function that obtains/returns the matrix to be acted on
## the setinv part sets the inverse (inv) to the required solution that is either already cached or is to be computed
## the getinv then returns this verified inverse that is NOT null
## the entire function finally returns a list of the sub-functions to be called any time
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(answer) inv<<-answer
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve first checks if the matrix x already has a cached solution by checking its getinv method
## if null is not returned then it returns the getinv
## otherwise the inverse is computed using the solve function and assigned to variable answer
## this variable is then set as the inverse using the "setinv" function
## this inverse is returned 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return (inv)
}

## TEST
##  a <- makeCacheMatrix()
##  > a$set(matrix(1:4,2,2))

##  FIRST RUN NOT CACHED:
##  > cacheSolve(a)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
##  CACHED:
##  > cacheSolve(a)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

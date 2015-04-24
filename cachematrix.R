## Put comments here that give an overall description of what your
## functions do
##Funcitons below help in calculating the inverse of a vector if already not calculated. 
##If calculated already gets the inverse from the cached data

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
#a.set the value of the vector b.get the value of the vector c.set the value of inverse  d.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
#The following function calculates the inverse  the special "vector" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}

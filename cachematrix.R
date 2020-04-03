## There are two functions in this process
## The first function function creates a special "matrix" object that can cache its inverse.

## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.


#The belwo function, makeVector creates a special matrix, which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
  

makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
    set <- function(y) 
    {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


# The following function calculates the mean of the special matrixeated with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


myMatrix<-makeCacheMatrix(matrix(1:4,2,2))

myMatrix$get()

myMatrix$getInverse()

cacheSolve(myMatrix)

myMatrix$getInverse()


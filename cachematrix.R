## The following two functions used together will calculate the inverese of a matrix and place that inverse into 
## cashed memory for later use

# The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix inverse
# 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) { # initializes x as a function 
    m <- NULL # setting M to NULL to be used by later code
  set <- function(y) { # set a function with the argument of y - allows use of cachesolve without re-running makeCacheMatrix
    x <<- y # sets x (in the parrent environment) to the value of y
    m <<- NULL # sets m (in the parrent environent) to NULL (this all relates to the cachemean call)
  }
  get <- function() x # creates a function "get" with no arguments that just gets the value of x from the parent environment
  setinv <- function(inv) m <<- inv # sets m (in the parrent environment) to the value of the "inv" arrgument 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function calculates the inverse of the matrix
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



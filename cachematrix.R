## This creates a simple extension to a matrix that retains the results of an
## inversion calculation after it is computed.  The cached results are 
## invalidated each time the matrix is changed.
##
## This is intended to be used as follows:
## randMatrix <- matrix(rnorm(100), 10)
## psdMatrix <- randMatrix %*% t(randMatrix)
## 
## valueInverse <- cacheSolve(psdMatrix) # computes the inverse
## valueInverse <- cacheSolve(psdMatrix) # uses cached inverse
## valueInverse <- cacheSolve(psdMatrix) # uses cached inverse
##

## This function creates an environment containing x and xInverse as matricies
## as well as set, get, setInverse, and getInverse as functions.  This turns
## it into something similar to an object in C++.  set/get affect x, and 
## setInverse/getInverse affect xInverse.  set will also set xInverse to a 
## special token "NULL".

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL

  ## set method will set xInverse to a special token, and set x to a new value.
  ## It might make sense to add a check for newMatrix != x, or some other 
  ## method to avoid invalidating the cached copy of x.  
  set <- function(newMatrix) {
    ## The <<- assignment operator searches the parent environment for a name
    ## before creating one in the local environment.
    x <<- newMatrix 
    xInverse <<- NULL
  }

  ## get method just returns the matrix.
  get <- function() {
    x
  }

  ## setInverse doesn't run the inversion function, it just saves the results
  ## to xInverse.
  setInverse <- function(newInverse) {
    xInverse <<- newInverse
  }

  ## getInverse just returns xInverse
  getInverse <- function() {
    xInverse
  }

  ## Returns a list with all of the functions.
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
    
}


## This function takes a "list" which is returned from makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if (!is.null(xInverse)) {
    ## I'm only printing out the message because it is done in the example.
    ## In an application, it would probably be undesirable as users don't
    ## care about seeing low-level details.
    message("getting cached data")
    return(xInverse)
  }
  ## Implicit else, as the if statement contains "return".
  data <- x$get()
  ## Run solve on the data.  The ... portion allows additional options to be
  ## passed to the solve function.
  xInverse <- solve(data, ...)
  # Now save the result (recall that setInverse doesn't actually call solve)
  x$setInverse(xInverse)
  # return the new inverse
  xInverse
}


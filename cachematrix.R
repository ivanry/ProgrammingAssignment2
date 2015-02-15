## These functions are intended to use for:
## 1. Creation of special object containing matrix and its inverted value
## 2. Retrieval of matrix inverted value: 1st time - calculated, 2nd and etc. time - cached


## Creates matrix wrapper containing following properties:
## -value
## -cache for matrix invertion

makeCacheMatrix <- function(x = matrix()) {

  ## Init cache value with NULL
  inverted <- NULL
  
  ## Matrix setter
  set <- function(y) {
    ## Set value
    x <<- y
    ## Reset cached matrix inversion
    inverted <<- NULL
  }
  
  ## Matrix getter
  get <- function() x

  ## Setter for matrix invertion cache value
  setInverted <- function(value) inverted <<- value

  ## Getter for matrix invertion cache value
  getInverted <- function() inverted

  ## Define function contract
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}




## This function computes the invertion for matrix wrapper created by makeCacheMatrix.
## In case of first call causes inveriotn computation.
## For further calls - returns cached values

cacheSolve <- function(x, ...) {

  ## Get cache value
  inverted <- x$getInverted()

  ## If cached value is present, return it
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }

  ## Get original matrix
  data <- x$get()
  
  ## Invert the matrix
  inverted <- solve(data, ...)
  
  ## Set result to cache
  x$setInverted(inverted)

  ## Return a matrix that is the inverse of 'x'
  inverted
}

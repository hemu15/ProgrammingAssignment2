## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
mat_inv <- NULL # create an empty matrix
  set <- function(y){ # set function to be accessed in case you need to set the value of the matrix
    x <<- y
    mat_inv <<- NULL
  }
  get <- function(){x} # get function to access the value of the matrix by cacheSolve function
  setinv <- function(inv){ # function to be accessed by cachesolve to set the inverse of the matrix incase it is not already set
    mat_inv <<- inv
  }
  getinv <- function(){mat_inv} #function to be accessed by cachesolve to get the inverse of the matrix incase it is not already get
  list(set = set,get = get,setinv = setinv,getinv = getinv) # list of all the functions to be accessed by cachesolve
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mat_inv <- x$getinv() #get and assign the inverse of the matrix to a variable whether null or not
  if(!is.null(mat_inv)){ # check if the inverse has already been got and cached earlier, if yes return the matrix inverse
    message("Getting Cached data")
    return(mat_inv)
  }
  data <- x$get() # in case inverse has not been calculated, get and assign the matrix to data variable
  mat_inv <- solve(data,...) # use solve fuction to get the inverse
  x$setinv(mat_inv) # set the inverse using superassignment so as to cache the data for future use
  mat_inv # return the matrix inverse
}

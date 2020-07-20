## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  in_ <- NULL
  set <- function(y){
    x <<- y
    in_ <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) in_ <<- inverse
  getInverse <- function() in_
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## following function calculates the inverse of the special "matrix" created with the above function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  in_ <- x$getInverse()
  if(!is.null(in_)){
    message('getting cashed data')
    return(in_)
  }
  
  data <- x$get()
  in_ <- solve(data, ...)
  x$setInverse(in_)
  in_
}

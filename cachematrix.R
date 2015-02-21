
## The following functions create a special object that stores a matrix and caches its inverse


## makeCacheMatrix creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## define cache inv
  inv <- NULL

  ## matrix set/get functions
  set <- function(y) {
    x <<- y       ## assign input matrix y to variable x in parent environment
    inv <<- NULL  ## re-initialize inv variable in parent environment to null 
  }
  get <- function() x  ## returns matrix x   
  
  
  ## inverse set/get functions
  setinverse <- function(inverse) {
    inv <<- inverse    ## assign input inverse to variable inv in parent environment    
  }
  
  getinverse <- function() inv ## returns cached inverse inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## get inverse cache
  inv <- x$getinverse()

  ## check if inverse cache has data, if so print message and return data
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }

  ## get input matrix
  data <- x$get()
  
  ## create inverse of matrix
  inv <- solve(data)
  
  ## add inverse to cache
  x$setinverse(inv)
  
  ## return inverse
  inv
}






















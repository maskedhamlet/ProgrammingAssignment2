## Caching Matrix Inversion

## The below function will create a matrix object that can cache its inverse.
## Similar to the example given
## set the matrix
## get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<- inverse
  getinv <- function() inv
  list(set=set,get=get, setinv=setinv,getinv=getinv)

}


## The following function calculates the inverse of the matrix from the above function,but checks
## to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mdata<-x$get()
    inv<-solve(mdata, ...)
  x$setinv(inv)
  return(inv)
          
}

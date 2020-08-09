##These functions create a matrix an calculate its
## inverse. Note: the matrix must be small in order
## for the functions to work.

## makeCacheMatrix creates a special matrix which is 
## actually a list of characteristics

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve takes the matrix and calculates its
## inverse. The introduced matrix should be square.

cacheSolve <- function(x, ...) {
  inv<-x$getmean()
  if(!is.null(inv)){
    message("geting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

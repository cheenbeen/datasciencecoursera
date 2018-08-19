makeCacheMatrix <- function(x=matrix()){
  
  inv<-NULL
  set <- function(y){
    
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(mean) inv<<-inverse
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x,...){
  
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get
  i<-inverse(data,...)
  x$setinv(i)
  i
}

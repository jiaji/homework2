makeCacheMatrix <- function(x = matrix()) {
s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x
  setsolve<-function(inverse) s<<-inverse
  getsolve<-function() s
  list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)

}


cacheSolve <- function(x, ...) {
s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
        ## Return a matrix that is the inverse of 'x'
}

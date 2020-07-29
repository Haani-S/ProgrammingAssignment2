#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse


#makeCacheMatrix creates a special "matrix"
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of matrix created with the above function makeCacheMatrix
# cacheSolve first check if the inverse is already calculated.
#If so, it gets the inverse from the cache directly 
#if not ,it calculates the inverse of the given matrix and sets its in the cache via the setinverse function.



cacheSolve <- function(x, ...) {            
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setinverse(inv)
  inv
}  
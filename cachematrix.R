## The following functions create a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

# initializes the value of the matrix inverse to NULL.
  m<-NULL
  set<-function(y){
    x<<-y
    
# changes the value of the matrix inverse in case the matrix has changed.   
    m<<-NULL
  }
# gets value of inverse matrix.  
  get<-function() x

# calculates the inverse of the matrix using the solve function.
  setmatrix<-function(solve) m<<- solve

# gets the calculated inverse   
  getmatrix<-function() m
 
# passes the value of the function makeCacheMatrix 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}

# used to get the cached matrix inverse if already calculated
cachesolve <- function(x, ...) {
  m<- x$getmatrix()

#if the matrix inverse exists, R gets it from cache.  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
 
#if the matrix inverse does not exist, R calculates it.  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
        
}


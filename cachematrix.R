## cachematrix takes a matrix and runs an inversion on it using solve(matrix)
## this will automatically cache the result so it only needs to
##run the inversion once

## makeCacheMatrix will create the matrix for inversing,
##thiss where the CasheMatrix will store the precomputed matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## cacheSolve will run on the marix from makeCacheMatrix
##but will also check to see if the matrix has alreddy ran or not

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x' 
        ##if the value of the matrix is null
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}


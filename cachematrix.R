## makeCacheMatrix will create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
             m<-NULL
             set<-function(y){
                   x<<-y
                   m<<-NULL
             }
             get<-function() x
             setmatrix<-function(solve) m<<-solve
             getmatrix<-function() m
             list(set=set,get=get,
                  setmatrix = setmatrix,
                  getmatrix = getmatrix)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
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

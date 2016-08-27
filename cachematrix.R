## These functions written in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; 27 August, 2016, Username: vseykov;



makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a  matrix that can cache its inverse
  m<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() {
    x
  }
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ##  Get the inverse of the matrix
  getInverse <- function() {
    m
  }
  ## List of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
    
        data <- x$get()
        
        m <- solve(data) %*% data
       
        x$setInverse(m)
      
        m      
}

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                           
    set <- function(k) {                    
      x <<- k                            
      inv <<- NULL                        
    }
    get <- function() x                     
    
    sinverse <- function(inverse) inv <<- inverse  
    ginverse <- function() inv                    
    list(set = set, get = get, sinverse = sinverse, ginverse = ginverse)
  }
  
  cacheSolve <- function(x, ...) {
    inv <- x$ginverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$sinverse(inv)
    inv
  }
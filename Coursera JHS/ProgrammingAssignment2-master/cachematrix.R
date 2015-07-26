## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 
  m_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }

  get <- function() x  
  set_in <- function(inv) m_inverse <<- inv
  get_in <- function() m_inverse

  return (list(set = set, 
               get = get,
               set_in = set_in,
               get_in = get_in
  )
  )
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  inv <- x$get_in()
  if(!is.null(inv)){
    message("retriving data")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data,...)
  
  x$set_in(inv)
  
  inv
}


#test code : http://www.mathsisfun.com/algebra/matrix-inverse.html
#foo <- c(4,7,2,6)
#dim(foo) <- c(2,2)
#bar <- makeCacheMatrix(foo)
#print (cacheSolve(foo))





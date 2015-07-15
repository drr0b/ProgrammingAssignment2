## This set of functions will create a matrix in such a way that
## the inverse matrix may be solved then cached.

## The first step is to create a matrix that contains a list of 
## functions allowing the caching.. this is makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {
  
  ## First set the inverse matrix as null by default
      inv <- NULL
      
  ## Now give option to set matrix
      set_matrix <- function(y = matrix()) {
                  inv <<- NULL
                  x <<- y
      }
  ## get_matrix just prints out the stored value
      get_matrix <- function() x
      
  ## set_inv allows the inverse matrix to be set
      set_inv <- function(inverse) inv <<- inverse
      
  ## similarly, get_inv prints stored value
      get_inv <-function() inv
      
    
## This bit attaches the functions to the list, allowing calling
## Using x$get_matrix, x$set_matrix etc.. It also returns the 
##final value
      list(get_matrix = get_matrix, set_matrix = set_matrix,
           get_inv = get_inv, set_inv = set_inv)
}


## This will retrieve the the inverse matrix if cached, or solve
## it on the fly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

## First get inv to see if it's cached  
        inv <- x$get_inv()
        
## If null get value
        
        if(is.null(inv)){
                        x$set_inv(solve(x$get_matrix()))
                        inv <- x$get_inv()
                        return(inv)
        }
        
        message("Returning cached inverse matrix")
        inv
        
}

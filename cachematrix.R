## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix() function creates a special vector which sets the initial value of 
#'inv' of matrix globally and also defines activities of various functions

# Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL             #initialize inv value as NULL locally
    set <- function(y) {
      x <<- y
      inv <<- NULL          #initialize inv value as NULL globally
    }
    get <- function() x     #get the existing value of x
    set_inv <- function(inverse) inv <<- inverse  #assign 'inverse' to 'inv' value globally
    get_inv <- function() inv                     #get the existing 'inv' value  
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}


## Write a short comment describing this function
#This functions checks if the inverse has been already calculated or not
#if calculated then it returns it otherwise it calculates and assigns it globally to
#a variable and prints it also

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    #Assuming x$get_inv() syntax works 
    inv <- x$get_inv()                #get the inverse of matrix assigned by calling get_inv matrix
    if(!is.null(inv)) {                #if 'inv' is not empty return the value of 'inv'
      message("getting cached data")
      return(inv)
    }
    data <- x$get()          #get the invertible matrix assigned to data
    inv <- solve(data, ...)  #calculate inverse of invertible matrix using solve function
    x$set_inv(inv)           #set value of 'inv' globally through <<- operator
    inv                      # return 'inv' of the matrix
  
}

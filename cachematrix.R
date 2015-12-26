## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL #assigns null to a variable i, that would be later used to store the inverse
     
     set <- function(y) {#set function sets the value of the matrix
          x <<- y #assign value in a parent environment(makeCacheMatrix) for the matrix
          i <<- NULL #assign value in a parent environment(makeCacheMatrix) for the inverse of the matrix
     }
     
     get <- function() x#calls the value of the matrix
     
     setinv <- function(inv) i <<- inv#function that assigns the value of the inverse of the matrix
     getinv <- function() i#function that calls the value of the inverse of the matrix
     
     list(set = set, #A list is needed so that we can assign makeCacheMatrix to an object that can store the 4 subfunctions 
          get = get,
          setinv = setinv,
          getinv = getinv
          )
}
## Write a short comment describing this function

cacheSolve <- function(x) {#function that returns the value or the inverse of the matrix if the value has not been calculated.
                           #If the value of the inverse has not been calculated it calculates it and returns it.
        
     i <- x$getinv()#gets the value of i
     if(!is.null(i)) {#checks if value of i is null
          message("getting cached data")#outputs a string if i is null
          return(i)
     }
     
     data <- x$get()#assigns the matrix to data
     i <- solve(data)#gets the inverse of the matrix
     x$setinv(i)#sets the value of the inverse of the matrix to i
     i
}


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
#inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.


## Function makeCacheMatrix creates a special "matrix" object and caches its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.

#The set function stores the matrix stored in the main or parent function.
#The get function returns the matrix stored in the main or parent function function.

#setinverse function calculates the inverse of matrix and stores result in m in the main or parent function
#getinverse function returns the inverse of matrix stored in m in the main or parent function

#The fumction returns a list containing the 4 enclosed functions

#Note: The solve function is used in the makeCacheMatrix function.  The solve function will compute the inverse of a square matrix.
#In other words use of this function to calculate inverse limits the makeCacheMatrix function usable only for square invertible matrices.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve accepts as input an argument which has been assigned the result of function makeCacheMatrix above. 
#Then cacheSolve first checks to see if inverse has already been calculated. If TRUE then the function displays message "getting cached data" then retrieves the inverse from the cache. 
#If FASLE, i.e the inverse has not been calculated, data is assigned then cacheSolve gets the matrix stored with makeCacheMatrix, then calculates the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#The following test case was done to illustrate function workings

#> x = rbind(c(1, 0.5), c(0.5, 1))
#> i<-makeCacheMatrix(x)
#> i$get()
#     [,1] [,2]
#[1,]  1.0  0.5
#[2,]  0.5  1.0


#> cacheSolve(i)   First Run, NO CACHE
#     [,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333


#> cacheSolve(i)  Second Run, CACHE FOUND and DISPLAYS, Note message "getting cached data"
    #getting cached data
#     [,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.333333



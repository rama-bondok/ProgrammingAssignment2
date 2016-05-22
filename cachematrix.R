##Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly.


##This function creates a special "matrix" object that can cache its inverse. It does the following
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
	inverse <- Null
	set <- function(y){
			x <<- y
			inverse <<- Null
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
	

}


##his function computes the inverse of the special "matrix" returned by makeCacheMatrix .
##If the inverse has already been calculated then the cachesolve should retrieve the 
##inverse from the cache.
cacheSolve <- function(x, ...){
	inverse <- x$getinverse()
	if(!is.null(inverse){
		message("getting the cached data of the inversed matrix")
		return(inverse)
	
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
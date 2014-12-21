## The following pair of functions cache the inverse of a matrix
## The first function creates a special matrix which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## The second function checks if the inverse of the matrix is already computed. If yes, it fetches the cached data else it calculates the inverse


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
	i <- NULL
      set <- function(y) {
      	x <<- y
            i <<- NULL
      }
      get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function checks if inverse is already cached. If not, it calculates the the inverse.

cacheSolve <- function(x,...) {
	inv <- x$getinv()
	if(!is.null(inv))
	{
		message("Fetching cached data...")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
	
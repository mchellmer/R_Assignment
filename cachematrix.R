## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This first function returns a special list which entries: set= assigns 
## a matrix x; get = retrieves a matrix x; setinv = stores the inverse of
## a matrix x; getinv = retrieves the calculated inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
		set <- function(y) {
			x <<- y
			I <<- NULL
		}
		get <- function() x
		setinv <- function(solve) I <<- solve
		getinv <- function() I
		list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## the function searches x for a cached inverse matrix, if no matrix
## is cached one is calculated from x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}

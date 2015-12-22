## Put comments here that give an overall description of what your
## functions do

## Greates and enhanced matrix object with carries functions for setting ang getting its value
## and setting/getting its inverse

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL 			# set the value of "inverse x" to NULL
        set <- function(y) {	#NOTE: the <<- operator is required because x and invx are outside the scope of "set"
                x <<- y			# set x to the value of argument y passed to the "set" function		
                invx <<- NULL		# resets "inverse x" to NULL, this is important because when x set to a new value
						# we won't want to return the old cached value of the inverse
        }
        get <- function() x		# return the value of matrix x
        setinv <- function(inv) invx <<- inv	#set the value of invx to "inv" which is passed as an argument
        getinv <- function() invx	# returns the current value of invx, could be NULL or could be already calculated
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of x where x is an object created by makeCacheMatrix
## if the inverse of x is already computed once, it will use the cached data
## and will not repeat computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx
}

#Example:
# > A <- makeCacheMatrix()
# > A$set( matrix(c(1,2,-1,4),nrow=2,ncol=2) )
# > A$get()
#      [,1] [,2]
# [1,]    1   -1
# [2,]    2    4
# > cacheSolve(A)
#            [,1]      [,2]
# [1,]  0.6666667 0.1666667
# [2,] -0.3333333 0.1666667
# > cacheSolve(A)
# getting cached data
#           [,1]      [,2]
# [1,]  0.6666667 0.1666667
# [2,] -0.3333333 0.1666667
# > invA <- A$getinv()
# > x <- A$get()
# > x %*% invA
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 



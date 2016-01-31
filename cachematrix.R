
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        #get the value of the matrix
        get <- function() x
        #set the value of inverse matrix
        setInverse <- function(solve) i <<- solve
        #get the value of inverse matrix
        getInverse <- function() i
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        }
        
cacheSolve <- function(x, ...) {
        #i is a value of the inverse matrix
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        data <- x$get()
        i <- solve(data) %*% data
        #code to get inverse matrix
        x$setInverse(i)
        }

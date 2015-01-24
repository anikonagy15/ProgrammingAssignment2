## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        stored <- NULL
        setelement <- function(a, b, c) {
                x[a,b]<-c
                x <<- x
        }
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) {
                m <<- solve
        }
        getSolve <- function() m
        setStored <- function (k) {
                stored <<- k
        }
        getStored <- function () stored
        list(setelement=setelement, set=set, get=get, 
             setSolve=setSolve,
             getSolve=getSolve,
             setStored=setStored,
             getStored=getStored)
}
## Write a short comment describing this function
cacheSolve <- function(j, ...) {
        ## Return a matrix that is the inverse of 'x'
        t <- j$getSolve()
        if(!is.null(t) && identical(j$getStored(), j$get())) {
                        message("getting cached data")
                return(t)
        }
        data <- j$get()
        t <- solve(data, ...)
        j$setSolve(t)
        j$setStored(data)
        t
}
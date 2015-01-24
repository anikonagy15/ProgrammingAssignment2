# We are using three matrices:
# x - the matrix that is initialized with makeCacheMatrix()
# y - the matrix whose inverse is stored in m
# m - the inverse matrix of y
# NOTE: 
# To check if the matrix, whose inverse matrix is stored in m
# is changed after the inverse was calculated with chaceSolved(), I 
# introduced y matrix. This matrix is always containing the matrix
# whose inverse is stored in m.
# If you reinitialized your object with the set() function, there
# is no need for this extra check. But if you modify only one element,
# and don`t recalculate the inverse, it is neccessary to check whose inverse
# is stored in the cache. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        y <- NULL
        # modifies the element of the matrix
        setelement <- function(a, b, c) {
                x[a,b]<-c
                x <<- x
        }
        # updates the x matrix and makes m null
        setx <- function(z) {
                x <<- z
                m <<- NULL
        }
        # returns x matrix
        getx <- function() x
        # updates m, which is the inverse of y matrix
        setSolve <- function(r) {
                m <<- r
        }
        # returns m that is the inverse of y matrix
        getSolve <- function() m
        # updates the y matrix
        sety <- function (k) {
                y <<- k
        }
        # returns the y matrix
        gety <- function () y
        list(setelement=setelement, setx=setx, getx=getx, 
             setSolve=setSolve,
             getSolve=getSolve,
             sety=sety,
             gety=gety)
}
# Calculates and prints the inverse matrix of the initialized matrix.
# Also updates y accordingly.
cacheSolve <- function(j, ...) {
        # puts m into t
        t <- j$getSolve()
        # if m not null and the original matrix is not changed, 
        # returns with the cached data
        if(!is.null(t) && identical(j$gety(), j$getx())) {
                message("getting cached data")
                return(t)
        }
        # if we are here, that means that m was null,
        # or the original matrix was changed,
        # let`s calculate the new inverse matrix and stores the new values
        data <- j$getx()
        t <- solve(data, ...)
        j$setSolve(t)
        j$sety(data)
        t
}

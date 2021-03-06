##Functions to make special cacheMatrix object and cache solve function


##makeCacheMatrix makes an object and returns a list containing functions to interact with object
##the object stores a matrix in x and the value of its inverse(if calculated already) in I
##if the inverse has never been computed, the value of I is null
##if the value of the matrix is changed by using the set function, I is set to null
#the returned functions are set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##cacheSolve is the solve function for cacheMatrix objects
##returns inverse value if found in x
##if inverse not computed already, it will be computed and stored in x
##ensures inverse not computed more than once for same matrix

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        I<- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}

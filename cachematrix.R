
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It has 4 functions within viz set,get,setinverse and getinverse - the actual function description are present in inline comments
##cacheSolve: This function takes in a list as argument. If the inverse of matrix is already cached,
##it gets the value from cache without calculation, if not, it calculates the inverse and sets the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ##This function sets the matrix in cache and the inverse as null in cache
                x <<- y
                m <<- NULL
        }
        get <- function() x ##returns the matrix
        setinverse <- function(solve) m <<- solve ## Gets the inverse of matrix and sets in the cache
        getinverse <- function() m ##Retreives the inverse from cache and returns
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## First, get the inverse of matrix if already calculated and present in cache
        m <- x$getinverse()
        ## Next, if the inverse value is not null, return the value (from cache) without solving
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If cache is null, inverse is calculated below using solve and it is set to cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. The default value is an identity matrix of size 2.
makeCacheMatrix <- function(m=diag(2)){
        inv <- NULL
        setmatrix <- function(n){
                m <<- n
                inv <<- NULL
        }
        getmatrix <- function() m
        setinverse <-function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = setmatrix,
             get = getmatrix,
             getinverse = getinverse,
             setinverse = setinverse)
}


# cacheSolve function uses the special "matrix" created using the above function  
# and calculates the inverse and if the inverse is already calaculated, it will 
# get the inverse from cached memory instead of recalculating only if the matrix
# is not changed


cacheSolve <- function(x){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        store <- x$get()
        inv <- solve(store)
        x$setinverse(inv)
        inv
}
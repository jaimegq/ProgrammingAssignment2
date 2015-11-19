## This function creates a special "matrix" object that can cache its inverse
## It returns a list of functions actually
## x needs to be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
                inv_matrix <- NULL
                set_matrix <- function(y) {
                        x <<- y
                        inv_matrix <<- NULL
                }
                get_matrix <- function() x
                set_inv_matrix <- function(solve) inv_matrix <<- solve
                get_inv_matrix <- function() inv_matrix
                list(set = set_matrix, get = get_matrix,
                     setinverse = set_inv_matrix,
                     getinverse = get_inv_matrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then it retrieves the inverse from the cache
## If the input matrix changes, it computes the inverse again

cacheSolve <- function(x, ...) {
                inv_matrix <- x$getinverse()
                if(!is.null(inv_matrix)) {
                        message("getting cached data")
                        return(inv_matrix)
                }
                data <- x$get()
                inv_matrix <- solve(data, ...)
                x$setinverse(inv_matrix)
                inv_matrix
}

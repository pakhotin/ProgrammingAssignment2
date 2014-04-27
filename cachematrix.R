# General comments:
# Matrix inversion is usually a costly computation and their
# may be some benefit to caching the inverted matrix rather
# than compute it repeatedly. Below pair of functions are
# defined that cache the inverted matrix and access it without
# recomputing once input matrix is inverted.

# Example:
# 1.1. Define square matrix 2x2:
#    > A = matrix(c(1, 2, 3, 4), nrow=2, ncol=2, byrow = TRUE)
# 1.2. Print matrix
#    > A
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 2. Create special "matrix":
#    > B<-makeCacheMatrix(A)
# 3.1. Invert special "matrix":
#    > C<-cacheSolve(B)
# 3.2. Print inverted matrix:
#    > C
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 4. Test that A*C = E, where E is a unit matrix
# | 1 2 |   | -2.0  1.0 |   | 1 0 |
# | 3 4 | * |  1.5 -0.5 | = | 0 1 |

# The "makeCacheMatrix" function creates a special "matrix",
# which is really a list containing the following functions
#   * "set"        - set the "value" of the matrix
#   * "get"        - get the "value" of the matrix
#   * "setInverse" - set the "value" of the inverse matrix
#   * "getInverse" - get the "value" of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inverse <<- Inverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# The "cacheSolve" function inverses the special "matrix" objetc
# created with the "makeCacheMatrix" function (defined above).
# However, this function first checks if the inverted matrix has
# already been calculated and the matrix has not changed:
#   * if so, it gets the inverted matrix from the cache and skips
#     the computation
#   * otherwise, it calculates the inverted matrix of the data and
#     sets the "value" of the inverted matrix in the cache via the
#     "setInverse" function
cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}

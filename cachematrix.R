
## Assignment

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.


## Function makeCacheMatrix :
## Creates a functions list that manage caching of one inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solve) m <<- solve
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}

## Function cacheSolve :
## Uses Function makeCacheMatrix() to calculate the inverse of a matrix 
## if it has not been cached before  


cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}

# Test Case :

# Invertible Matrix

# Matrix<-diag(2,10)
# Matrix

# Output
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    4    0    0    0    0    0    0    0    0     0
# [2,]    0    4    0    0    0    0    0    0    0     0
# [3,]    0    0    4    0    0    0    0    0    0     0
# [4,]    0    0    0    4    0    0    0    0    0     0
# [5,]    0    0    0    0    4    0    0    0    0     0
# [6,]    0    0    0    0    0    4    0    0    0     0
# [7,]    0    0    0    0    0    0    4    0    0     0
# [8,]    0    0    0    0    0    0    0    4    0     0
# [9,]    0    0    0    0    0    0    0    0    4     0
# [10,]    0    0    0    0    0    0    0    0    0     4

# Cache <- makeCacheMatrix(Matrix)
# cacheSolve(Cache)

# Output
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,] 0.25 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
# [2,] 0.00 0.25 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
# [3,] 0.00 0.00 0.25 0.00 0.00 0.00 0.00 0.00 0.00  0.00
# [4,] 0.00 0.00 0.00 0.25 0.00 0.00 0.00 0.00 0.00  0.00
# [5,] 0.00 0.00 0.00 0.00 0.25 0.00 0.00 0.00 0.00  0.00
# [6,] 0.00 0.00 0.00 0.00 0.00 0.25 0.00 0.00 0.00  0.00
# [7,] 0.00 0.00 0.00 0.00 0.00 0.00 0.25 0.00 0.00  0.00
# [8,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.25 0.00  0.00
# [9,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.25  0.00
# [10,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.25


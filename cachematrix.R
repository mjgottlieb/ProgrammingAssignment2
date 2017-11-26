## The following functions utilitze 'caching' to solve for the inverse of a matrix.
## This approach helps speed up processing time by splitting up the required CPU work 
## into separate, isolated efforts.


## makeCacheMatrix() takes a matrix 'x' as an argument and establishes a series 
## of equations that will be used with a later function to calculate the matrix's 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        B <- NULL
        set <- function(y){
                x <<- y
                B <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) B <<- inverse
        getInverse <- function() B
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

n <- matrix(floor(runif(16,1,10)),4,4)
q <- makeCacheMatrix(n)

## cacheSolve() takes the same matrix 'x' and computer its inverse using the cached functions
## established in the makeCacheMatrix() function

cacheSolve <- function(x, ...){
        B <- x$getInverse()         
        if(!is.null(B)){            
                message("getting cached data")
                return(B)
        }
        data <- x$get()
        B <- solve(data, ...)
        x$setInverse(B)
        B
}

z <- cacheSolve(q)

round(n%*%z)

# This is Giuliet Programming Assignment2 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix.
# It gives for result specification on the function's environment. 

makeCacheMatrix <- function (x=matrix()) {
        inv <- NULL
        set <- function (y){
                x<<- Y
                inv <<-NULL
        }
        
        get<-function() x
        setinverse <- function (inverse) inv <<- inverse
        getinverse <- function () inv
        list (set=set, get = get,
              setinverse= setinverse,
              getinverse = getinverse)
}  

# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed, through the 'if structure'. If so, it gets the result and skips the
# computation. If not, it computes the inverse, using the function solve () and sets the value in the cache via
# setinverse function

cacheSolve <- function (x,...){
        inv <- x$getinverse ()
        
        if (!is.null(inv)){
                message ("getting cached data")
                return (inv)}
        
        data <- x$get ()
        inv <- solve (data)
        x$setinverse (inv)
        inv
}

# Sample Run
# > x <- rbind(c(2/3, -3/8), c(-3/8, 2/3))
# > x
# [,1]       [,2]
# [1,]  0.6666667 -0.3750000
# [2,] -0.3750000  0.6666667
# > m<-makeCacheMatrix(x)
# > m
# $set
# unction (y) 
# {
#        x <<- Y
#        inv <<- NULL
# }
# <environment: 0x0000000013215ff8>
#        $get
# function () 
#        x
# <environment: 0x0000000013215ff8>
#        $setinverse
# function (inverse) 
#        inv <<- inverse
# <environment: 0x0000000013215ff8>
#        $getinverse
# function () 
#        inv
# <environment: 0x0000000013215ff8>

# > m$get()
# [,1]       [,2]
# [1,]  0.6666667 -0.3750000
# [2,] -0.3750000  0.6666667
# > cacheSolve(m)
# [,1]     [,2]
# [1,] 2.194286 1.234286
# [2,] 1.234286 2.194286
# > 

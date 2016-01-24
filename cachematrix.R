## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                     #initialize makeCacheMatrix as NULL
        set <- function(y) {                          #define set function
                x <<- y
                i <<- NULL
        }
        get <- function() x                           #define get function 
        setinverse <- function(inverse) i <<- inverse #define setinverse function 
        getinverse <- function() i                    #define getinverse function 
        list(set = set, get = get,                    #list all 4 functions
             setinverse = setinverse,
             getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
        i <- x$getinverse()                            #get the input inverse using getinverse function
        if(!is.null(i)) {                              #check if the inverse has already been calculated
                message("getting cached data")         #if yes, print message 'getting cached data'
                return(i)                              #return inverse in CacheMatrix
        }
        data <- x$get()                                #If not,get the input                 
        i <- solve(data)                               #caculate the inverse 
        x$setinverse(i)                                #put inverse in CacheMatrix using setinverse function
        i                                              #print the output
}
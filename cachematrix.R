# Coursera R Programming - Assignment Part 2

# The following functions calculate the inverse of a matrix and save it to the 
# cache. 

# The function makeCacheMatrix returns a list containing 4 functions:
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        # initialize the cache 
        
        m <- NULL
        
        # store the new matrix y into the variable x; since the matrix is 
        # assigned a new value, cache (m) is re-initalized 
        
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        
        # return the stored matrix
        
        get <- function() x 
        
        # store the inverse matrix into the cache
        
        setinverse <- function(solve) m <<- solve 
       
        # return the cached inverse of x
        
        getinverse <- function() m 
        
        # list the functions
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The function cacheSolve calculates the inverse of the matrix previously created
# If the inverse has already been caclulated, it gets the inverse from the cache
# and skips the computation. Otherwise, it calculates the matrix inverse
# and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of the matrix x
        
        m <- x$getinverse()
        
        # return the inverse if its already set
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Get the matrix from our object
                
        data <- x$get()  
        
        # Calculate the inverse using solve
        
        m <- solve(data, ...)
        
        # setting the inverse to the cache
                
        x$setinverse(m)
        
        #returns the matrix
        
        m
}

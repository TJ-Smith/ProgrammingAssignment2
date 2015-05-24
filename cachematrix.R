# Exercise in cache functions using lexical scoping
# Tom Smith 5/18/2015
# Usage:  
    # Construct a function to take in a square matrix x
    # Call makeCacheMatrix(x), assign result to variable
    # Call cacheSolve (variable) to return the inverse, either calculated or from cache
    
makeCacheMatrix <- function(x = matrix()) {
    # set up the cache environment
    # x: a square matrix
    # return: a list containing functions to:
            #set the incoming matrix
            #get the incoming matrix
            #set the matrix inverse
            #get the matrix inverse
        inv = NULL
        set = function(y) {
                x <<- y
                inv <- NULL
        }
        get = function() x
        setinv = function(inverse) inv <- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Tom Smith 5/18/2015

cacheSolve <- function(x, ...) {
    #set up the solve envronment
    #return the inverse of 'x'

        inv = x$getinv()
        
        # check if inverse exists
        if (!is.null(inv)){
                # get from the cache 
                return(inv)
        }
        
        # if inverse not exists:
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache
        x$setinv(inv)
        
        return(inv)
}



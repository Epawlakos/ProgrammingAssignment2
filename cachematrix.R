## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function is designed create a special "matrix object" 
## and can cache its inverse
## This funciton returns a list of functions 
makeCacheMatrix <- function(x = matrix()) {
        # set the initial cache to NULL
        inv <- NULL
        
        # store a matrix
        setMatrix <- function(y){
                x <<- y
                # set the cache to NULL since the matrix has a new value
                inv <<- NULL
        }
        
        # function to return the sotred matrix
        get <- function() x
        
        # function to set the inverse
        setInv <- function(inverse) inv <<- inverse
        
        # function to get the inverse
        getInv <- function() inv
        
        # return a list of the functions
        
        list(setMatrix = setMatrix, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## This function is designed to compute the inverse of the "matrix" created above
## The function checks if the inverse has been calculated already (and if so, gets it from 
## the cache) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        
        ## check if the inverse has already been calculated
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## if the cached value doesnt exist, calculate the inverse and store it in the cache
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        
        ## return the inverse of the matrix
        
        inv
}

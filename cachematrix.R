## Assignment 2 - Creating a cache of the inverse of a matrix
## Caching data can reduce computational load by avoiding doing the same 
## calculation repeatedly

makeCacheMatrix <- function(x = matrix())
{   ## Initialize the inverse 
    i <- NULL
    
    ## Set the matrix
    set <- function(y)
    {   x <<- y
        i <<- NULL
    }
    
    ## Get the matrix
    get <- function() 
    {   x
    }
    
    ## Set the inverse
    setInverse <- function(inverse) 
    {   i <<- inverse
    }
    
    ## Get the inverse
    getInverse <- function()
    {   i
    }    
    
    ## Return the list of methods
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computing the inverse of the matrix returned by the function 
## 'makeCacheMatrix' above. If the inverse is already calculated, then retrieve
## the inverse from the cache

cacheSolve <- function(z, ...) 
{   ## Return a matrix that is the inverse of z
    inv <- z$getInverse()
    
    ## Return the inverse if it is already computed
    if(!is.null(inv))
    {   print("Getting cached data")
        return(inv)
    }
    
    ## Otherwise, get the matrix 
    data <- z$get()
    
    ## Solve for the inverse matrix (if argument b is not defined, the function
    ## 'solve' assumes it is the identity matrix and the result is the inverse 
    ## matrix)
    inv <- solve(data)
    
    ## Set the inverse
    z$getInverse(inv)
    
    #Return the matrix
    inv
}

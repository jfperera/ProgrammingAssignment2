## Used together these functions will create an object that allows a matrix and its inverse to be stored and 
## retrieved.  The first function MakeCacheMatrix creates the object, along with variables and storage interface, while the second 
## function cacheSolve, uses the object created by MakeCacheMatrix to determine whether the inverse has already 
## been calculated.  If the inverse has not been calculated, it will find the inverse and store that matrix in the
## object for future use
## 


## MakeCacheMatrix takes a square matrix as an argument, and returns a list of functions that can be used to
## store and retrieve the original matrix as well as its inverse

MakeCacheMatrix <- function(x = matrix()) {      #forms the input data into a matrix
        inv <- NULL             #sets the inverse equal to null
        set <- function(y) {    #a function, that replaces the matrix that is initially passed to MakeCacheMatrix
                x <<- y         #with a new matrix
                inv <<- NULL    #and resets the inverse to null 
        }
        get <- function() x     #a function that returns whatever matrix is being evaluated
        setinverse <- function(inverse) inv <<- inverse    #a function that loads a computed 'inverse' into the cached 'inv' variable
        getinverse <- function() inv          #a function that returns whatever is in the cached 'inv' variable
        list(set = set, get = get,         #returns a list to the main environment where each of these functions
             setinverse = setinverse,      #is indexed according to their function name
             getinverse = getinverse)
}



## cacheSolve takes the output of MakeCacheMatrix as an argument, and will determine whether an inverse 
## is already stored in the CacheMatrix object, and return the inverse if it is present.  
## Otherwise it will calculate the inverse an store it in the CacheMatrix object, and return the inverse matrix

cacheSolve <- function(x, ...) {                 #takes the results of MakeCacheMatrix as an argument
        inv <- x$getinverse()                    #loads the results of get inverse into variable inv
        if(!is.null(inv)) {                      #if inverse matrix is stored it returns that matrix
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         #loads original matrix into "data"
        inv <- solve(data, ...)                 #calculates inverse of "data", stores in inv variable 
        x$setinverse(inv)                       #and sets inv in the special matrix object
        inv                                     #finally returns the inverse
}

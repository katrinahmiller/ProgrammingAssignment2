## Put comments here that give an overall description of what your
## functions
# makeCacheMatrix takes a matrix as a variable and returns 4 functions
# set - function to create a copy of the matrix and a dummy variable for the inverse
# get - returns the matrix
# setInv - caches the matrix inverse
# getInv - if the inverse has been calculated, returns the cached answer
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the internal value x to be the matrix
    #create the variable for cached matrix
    set <- function(y) {
        #x will contain a copy of the matrix
        x <<- y
        #m is intended for the cached inverse but begins as a null
        m <<- NULL 
    }
    #get returns x
    #Set the initial values for x amd m
    set(x)
    #Get the current version of x
    get <- function() x
    #Set the inverse
    setInv <- function(matrixInverse) m <<- matrixInverse
    #Get the inverse
    getInv <- function() m
    #Returns the list of functions.  Also returns x and m if set
    list(set = set, get = get, setInv = setInv, getInv = getInve)   
}
 
 
## Write a short comment describing this function
#The first time through the inverse will be computed and cached.
#All following calls will return the cached version
 
cacheSolve <- function(x, ...) {
  m <- x$getInv()
    if (!is.null(m)) {
        #This will be returned the second time through
        message("getting cached data")
        return(m)
    }
    #The first time through set the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
        ## Return a matrix that is the inverse of 'x'
    m
}
 
#Code to demonstrate
mat <- matrix(c(4,-8,2,-2,-1,1,-3,1,2),nrow=3,ncol=3)
example <- makeCacheMatrix(mat)
cacheSolve(example)
#prints out the inverse
cacheSolve(example)
#prints "getting cached data"
#then prints inverse

## Put comments here that give an overall description of what your
## functions do


##Creates an object function for setting, getting a matrix and
##setting, getting the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setix <- function(newix) ix <<- newix
    getix <- function() ix
    list(set = set, get = get,
         setix = setix,
         getix = getix)
}



##Calculates and sets the inverse of the object created by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getix()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data)
    x$setix(ix)
    ix
}


##Test case, m1 %*% n1 will be the identity matrix if
##n1 is the inverse of m1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)
n1 <- cacheSolve(myMatrix_object)

m1 %*% n1
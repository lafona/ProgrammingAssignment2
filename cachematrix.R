## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creates a list which contains the functions necessary for manipulation of the matrix x.
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<- NULL
    }
    get<- function () x
    setinv<- function(solve) m<<- solve
    getinv<- function() m
    list(set =set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Calculates the inverse of matrix 'x', using cached inverse data if the function has been run previously
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<- x$get()
    m<- solve(data, ...)
    x$setinv(m)
    m
}

## These 2 functions work jointly in order to calculate the inverse of a matrix(given as argument) and will then store it in the special object's (the 2 functions we created) environment.
## If the inverse of the given matrix had already been calculated
##and/or stored, then, the functions will only retrieve and return the inverse of the matrix.

## A function that consists of a list of 4 functions (that retrieves and set 2 objects) and 2 objects (a matrix and an object meant to store the inverse of the matrix if it has been calculated)

makecachematrix <- function(m = matrix()) {
  im <- NULL
  set <- function(x) {
    m <<- x
    im <<- NULL
  }
  get <- function() m
  setinversem <- function(setim) im <<- setim
  getinversem <- function() im
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem) 
}


## This function uses the previous function as arguments in order to retrieve the objects from the previous function.
##Depending on if the inverse matrix object has already been calculated and stored, this function will either calculate it and store it or retrieve it from its cache storage and return it.

cachesolve <- function(z, ...) {
  im <- z$getinversem()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- z$get()
  im <- solve(data, ...)
  z$setinversem(im)
  im
}
#testing the functions 
## Return a matrix that is the inverse of 'x'
w<-matrix(data = (c(1,99,3,44,6,7,2,14,10000)), nrow = 3, ncol = 3)
yo<-makecachematrix(w)
cachesolve(yo)  
##[,1]          [,2]          [,3]
##[1,] -1.377156e-03  1.011534e-02 -1.388605e-05
##[2,]  2.275928e-02 -2.297635e-04 -4.230187e-06
##[3,] -1.551835e-05 -2.873768e-06  1.000071e-04

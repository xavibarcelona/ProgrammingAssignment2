## On this assignment 2, we are using 2 diferent functions; "makeCacheMatrix" and "cachesolve".
## both functions are meant to calculate the inversion of a matrix, using R calculations
##using the solve() function in R. However, the first function has a unique feature that
##allows us to store the result in cache thanks to R lexical scooping mechanichs.
##Moreover, our second function is an example how to use those cache-stored-results,
##It reviews if the result was already calculated in our first function; if so, it just prints
##the result stored in the cache, if not, it calculates the result. This is meant to avoid
##repeating calculations and saving time (usefull when using big ammounts of data)

## This first function "makeCacheMatrix" creates a special "matrix" object 
##that is meant to cache its inverse, using the R function "Solve".

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {     ##Sets the value of the vector
    x<<-y
    m<<-NULL
  }
  get<-function() x        ##Gets the value of the vector
  setsolve<- function(solve) m<<-solve  ##Sets the value of the inverted matrix
  getsolve<-function() m ##Gets the value of the inverted matrix
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve) ##assign actions, specially for inheriting them on the next function
}


##  This function is meant to compute the inverse of the special "matrix" 
##returned by the first function. If the inverse was already calculated, 
##it will use last function saved cache to return the result. Otherwise, 
##it should calculate it on its own.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()     ##Tells the function to check if the inversion of the matrix
  if(!is.null(m)) {     ##has been already calculated, if so, returns the stored result
    message("getting cached data")
    return(m)
  }
  data<-x$get()         ##If the data hasn't been calculated by our first function therefore
  m<-solve(data,...)    ##not stored in the cache; it makes the calculation for us.
  x$setsolve(m)
  m
}

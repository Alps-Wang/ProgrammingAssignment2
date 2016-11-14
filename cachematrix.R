## Put comments here that give an overall description of what your
## functions do

## This function create a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ##function definition
    invs <- NULL                ## initialize invs as NULL
    set <- function(y){         ## set new "matrix" 
      x <<- y                   ## assign value of y to x
      invs <<- NULL             ## assign NULL to invs
    }
    
    get <- function() { x }     ## get function is to get value of x
    setinverse <- function(inverse) {invs <<- inverse} ## setinverse function is to assign inverse to invs
    getinverse <- function() { invs } ## getinverse function is to get the value of invs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse ) ## create a list to show the above four functions
} ## function end

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) { ## function definition    
   invs <- x$getinverse()       ## assigne inversed matrix to invs by getinverse function
   if(!is.null(invs)){          ## check if the inverse has been calculated
     message("getting cached data.") ## if calculated , print a message
     return(invs)               ## return invs 
   }
   ## if not being calculated
   data <- x$get()  ## assign data with the value of get function
   invs <- solve(data) ## inverse matrix and assign to invs
   x$setinverse(invs)  ## store the inversed matirx by setinverse function
   invs         ## Return a matrix that is the inverse of 'x'
} ## function end

# a function that create a functional list that checks if there is a stored version of a matrix inverse 
#and either returns the cached inverse or calculates the inverse and cashes that inverse before returning the inverse
makeCasheMatrix <- function( A = matrix())
{inv <- NULL # set inv to NULL in current environment
 set <- function(y){  # define set function which deep assigns y to a and inv to NULL
         A <<- y
         inv <<- NULL
 }
        get <- function() A # define get function to return matrix A
 setinverse <- function(solve ) inv <<- solve # define setinverse function to use solve function to get inverse and deep assign to inv
 getinverse <- function() inv      # define getinverse function 
list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
# define list of fuctions with defaults equal to self or to run function when list is referenced
}

casheSolve <- function(A, ...)
{
        inv  <- A$getinverse() #inv is assigned to the value of the getinverse function of functional list for A matrix   in the working environment
        if(!is.null(inv)) { # if inv is not NULL return message followed by cached inv value
                message("getting cached data")
                return(inv) 
        }
        data <- A$get()  # data is assigned to value of the get function from the A matrix functional in the working environment
        inv <-  solve(data, ...) # inv is asigned to value of solve function applied to data in the working environement
        A$setinverse(inv) #the setinverse function from functional list A is applied to inv
        inv # return value of inv in working environment
}

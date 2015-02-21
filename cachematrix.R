## This file contains two functions
## 1. makeCacheMatrix: 
#                       This function takes a matrix and returns a special vector,essentially a list containing 
#                       following: functions which  work on the input matrix                    
#                      a) get :return the matrix
#                      b) set: set a new value to matrix and set old cached inverse value to null
#                      c) getsolve: returns the cached inverse value of the matrix
#                      d) setsolve: set value to inverse for caching
##                      Usage: makeCacheMatrix(matrix)
##                      Arguments: 
#                               matrix - input matrix whose inverse is to be stored
## 2. cacheSolve:     
#                       This functions takes in the special vector generated from makeCacheMatrix
#                       and fetches the inverse value from it using getsolve. If the value of inverse
#                       is not null it returns the cached value after printing a message "getting cached
#                       data" else calculates the inverse of the matrix of the special vector (retrived)
#                       using get function and sets it in the cache using setsolve and returns the inverse
##                      Usage: cacheSolve(x,...). If the input matrix is not inversible an error is printed
##                       Arguments:
#                                x  special vector containing get and set functions for a matrix and its cached inverse   
#                                In addition a set arguments can be given which will be passed to the solve function
#                                used for calculation of uncached inverse
## Example
#
# >mat<-matrix(1:4,2,2)
# >invector<-makeCacheMatrix(mat)
# >cacheSolve(invector)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(invector) #on running second time inverse fetched from cache
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## makeCacheMatrix is function takes a matrix as input and creates a vector with list of methods
## to get the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        # set inverse of matrix to null
        inv<-NULL 
        # set function for setting matrix
        set <- function(y) {
                #matrix x set to have values of matrix y
                x <<- y 
                #set inverse to NULL as new matrix is set
                inv<<-NULL
                
        }
        # get function to return the matrix
        get<-function() x
        # setsolve function sets the inverse(solve) of the matrix
        setsolve<-function(solve)inv<<-solve
        # getsolve function returns the inverse value
        getsolve<-function()inv
        # list containing set and get methods for the matrix and its cached inverse
        list(set=set,get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns inverse of a matrix(part of input vector) from the input vector's cache, if cache is null 
## inverse is calculated and returned and stored in the cache( part of input vector)

cacheSolve <- function(x, ...) {
        # get inverse from output of makeCacheMatrix function 
        inverse <- x$getsolve()
        
        # if inverse in not null return the inverse after messaging  getting cached data
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## this section gets called when inverse is not present in cache
        # get the matrix
        data <- x$get()
        # call solve function to get inverse of input matrix
        inverse <- solve(data,...)
        # set the inverse in x so that it is cached and can be used next time.
        x$setsolve(inverse)
        # return inverse
        inverse
}

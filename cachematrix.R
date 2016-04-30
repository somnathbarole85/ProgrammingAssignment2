## This file contains two functions makeCacheMatrix & cacheSolve.
## The function makeCachemetrix creates a special matrix object that can cache its inverse.
## The function cachesolve computes the inverse of the matrix returned by the function
## makeCacheMatrix. If the inverse has already calculated & the matrix is not changed
## then the function cacheSolve will retrieve the inverse from cache.

## Please read below to get more about these two functions. For most of the
## command lines, comments are provided, I hope this will help reader to understand the program. 


# The function makeCachemetrix creates a special matrix object that can cache its inverse.
# It contains four functions set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
              
  inv <- NULL
  
  set <- function(y) {                              #set function chnages the matrix stored in 
                                                    #if we need to change the matrix
  x <<- y  
  inv <<- NULL
  
  }
  
  get <- function() x                               #get returns stored matrix
  setinverse <- function(inverse) inv <<- inverse   #setinverse stores the input inverse of matrix
  getinverse <- function() inv                      #getinverse returns the inverse of matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #list stores all 4 functions

}


#The function cachesolve computes the inverse of the matrix returned by above function
# makeCacheMatrix. If the inverse has already calculated & the matrix is not changed
# then the function cacheSolve will retrieve the inverse from cache.

cacheSolve <- function(x=matrix(),...) {
  
  inv <- x$getinverse()                    # inv stores x$getinverse
  
  if(!is.null(inv)) {                      #if inv is not null (or calculated previously), 
                                           #it returns inv with message getting cached data
    message("getting cached data")          
    return(inv)
    
  }
    
    mat_given <- x$get()                  # if inverse not calculated previously     
                                          # mat_given stores the given matrix and
   inv <- solve(mat_given)                # function solve calculates inverse of given matrix              
    
   x$setinverse(inv)                      # stores inverse of matrix in object setinverse                   
                                          
   inv                                    # returns inverse of matrix
    
  }
  
  


#Sample examples to verify output

a <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
#a <- makeCacheMatrix(matrix(c(0,5,99,66), 2, 2))
#a <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), 3, 3))

cacheSolve(a)








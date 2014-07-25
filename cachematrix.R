## Put comments here that give an overall description of what your
## functions do
# 
# The cachematrix.R module implements two functions for efficient computation of 
# inverse of a square matrix (assuming the matrix is properly conditioned and invertible)
# by caching and avoiding repetitive evaluations.
#
# The functions makeCacheMatrix and cacheSolve are briefly explained below.
#
#
# Author: Balaji Narayanan
# Date: 07/20/2014
# Modification history
#


## Write a short comment describing this function
#The makeCacheMatrix function makes and returns a cache matrix as a list 
#containing functions to set or initialize a data matrix, get the data matrix, 
#set and get an inverted matrix.
#
#Input x is a matrix
#Output is a list with 4 functions as objects
#
#
makeCacheMatrix <- function(x = matrix()) {
#makes a cache matrix 
  
  #initialize NULL
  m<-NULL
  
  #set data matrix
  set=function(y) {
  x <<- y
  m <<- NULL
}
#function to get data matrix
get<-function() x

#functions to set and get inverted matrix
setmatrix<-function(invmatrix) m <<- invmatrix
getmatrix<-function() m

#output list
list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)

}


## Write a short comment describing this function
#The cacheSolve function checks whether the inverse matrix has already been 
#calculated, in which case the function retrieves the inverted matrix from cache, 
#If not the function computes the inverse and sets it in cache.
#
#Input a matrix x created from the above function
#output inverted matrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  #get the data matrix
  m<-x$getmatrix()
  
  #if m is NULL inverse already computed retreive it from cache
  #and return it
  if(!is.null(m)){
    message("getting cached inverted matrix")
    return(m)
  }
  
  #get the data matrix and compute inverse
  nmatrix<-x$get()
  m<-solve(nmatrix) #inverse obtained through solve
  
  #set the inverted matrix in cache
  x$setmatrix(m)
  
  #return output
  m
}

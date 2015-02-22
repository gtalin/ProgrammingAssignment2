# Our program consists of 2 main functions: makeCacheMatrix() and cacheSolve()

#makeCacheMatrix: makes a modified "matrix" which is basically a list of 4 functions
##: set(), get(), setinv() and getinv(). This function creates a modified "matrix" for our 
# original matrix, extracts the original matrix
# and sets its inverse matrix when the inverse is calculated. It can also retrieve 
#the inverse matrix. 

#cacheSolve(): tries to see if the inverse of the matrix for which we want an inverse
# has already been calculated or not. If it is, it returns the inverse and if not, 
# it finds the inverse, sets it in our modified "matrix" so that it doesn't need to be
# recalculated in case it is needed again. 


#The respective function descriptions in detail:

##The makeCacheMatrix function first makes a new modified "matrix". 
# This modified "matrix" is a list of 4 functions: set(), get(), setinv() and getinv() 
# Modified matrix is created by calling the makeCacheMatrix(). 
# This will create a list of the above 4 functions.
# If no argument is passed to makeCacheMatrix(), this list will be created with an empty matrix.
# The set() function actually creates this modified "matrix" with the matrix of our choice.
# We need to call set() using the empty modified "matrix" that we created.
# Example: modifiedMatrix <- makeCacheMatrix(); modifiedMatrix$set(m);
# Where m is the matrix whose inverse we want
# get() function returns the original matrix (whose inverse we have to calculate)
# setinv() function sets the inverse value that is passed to it (to the variable inv) 
# getinv() function extracts the inverse value of our matrix. If an inverse has not 
# been set for that matrix, it returns NULL


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL#initialize inverse as NULL. 
  set <- function(y) {#y is the actual matrix passed to create a modified "matrix"
    
    #As a side note, we don't need to call the set() function if
    # we make the modifiedMatrix as modifiedMatrix <- makeCacheMatrix(x) where x is the matrix
    #to be cached. But that is not the right usage 
    
    x <<- y#The <<- operator insures that the value of y modifies the value of x 
    #(the parent variable) if we did x <- y, changes to x would not exist outside 
    #the set() function
    inv <<- NULL#The <<- operator helps make changes to the variable inv defined in the
    #parent function
  }
  get <- function() x#return x the original matrix whose inverse needs to be calculated
  setinv <- function(invVal) inv <<- invVal#The <<- operator makes changes to the variable inv defined in the
  #parent function. ELse changes to inv in this function would not have reflected in parent
  getinv <- function() inv#returns inverse of matrix
  #function returns a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


#Next is the cahceSolve() function. 
# We pass our modified "matrix" (list of functions) to the cahceSolve() function.
# cacheSolve function first tries to find if the inverse of the matrix is found or not
# It calls the function: getinv() on our passed "matrix"
# If inverse is not null (meaning the inverse has been calculated for that "matrix"  
# and has been set)
# We then return that inverse of our function.
# It is possible that a new modifed "matrix" has been made but its inverse value 
# has not been calculated so far. In that case, inv value would be NULL. 
# In that case, we would get the actual matrix, using get() function. 
# Then find the inverse value and using the setinv() function, we set the inverse 
# value of the modified "matrix" 
# so that in the future when we need the inverse of the matrix, it would
# not need to be recalculated

cacheSolve <- function(x, ...) {#x is the modified "matrix" which is really a list of the 
  #4 functions mentioned above.
  inv <- x$getinv()#If the inverse value of the matrix has been computed, this returns
  # that inverse value. Else returns NULL. 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()#get the actual matrix
  message("first time finding inverse")
  inv <- solve(data, ...)#finds inverse of the matrix
  x$setinv(inv)
  ## Return a matrix that is the inverse of data
  inv
}


# #Testing how the functions work
# m=rbind(c(1, -1/4), c(-1/4, 1))  #matrix for which inverse is required
# modifiedMatrix <- makeCacheMatrix()
# modifiedMatrix$set(m)
# 
# #first time cacheSolve is called, it calculates the inverse
# cacheSolve(modifiedMatrix)
# #second time it is called, it extracts from cache
# cacheSolve(modifiedMatrix)
# #The above answer will be the same as when we do solve(m)
# Just for a trial commit
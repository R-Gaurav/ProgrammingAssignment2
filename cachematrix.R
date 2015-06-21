## This file contains functions for caching the inverse of the matrix and 
## return it when the inverse of a matrix (not changed as of before) is
## demanded, however if there's a change in the matrix it computes the new inverse


## This function makeCacheMatrix() returns a list of functions for 
# setting the value of the matrix using set()
# getting the value of the matrix using get()
# setting the value of the inverse of the matrix using setinverse()
# getting the value of the inverse of the matrix using getinverse()

makeCacheMatrix <- function(x = matrix()) 
{

	m<- NULL  # variable to store the inverse
	set <- function(y)
	{
		x<<- y # '<<-' assigns a vlaue to an object in an environment other than the current enviornment
		m<<- NULL  
	}

	get <- function() x # return the matrix x
	setinverse <- function(inverse) m<<- inverse # assigns the inverse to variable m
	getinverse<- function() m # returns the inverse
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the inverse of an unchanged matrix exists 
## If it exists it return the inverse, if not it computes the new inverse and
## returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse() # return the inverse of the matrix

        if(!is.null(m)) # If the variable 'm' (inverse) is not null it returns the inverse
        {
        	message("getting cached data")
        	return m # return the inverse
        }

        ## However if inverse the matrix is null
        data <- x$get() # fetch the matrix
        m<- solve(data,...) # get the inverse via inbuilt solve()
        x$setinverse(m) # set the inverse (i.e. assgin the variable m in makeCacheMatrix to inverse)
        m # return the inverse
}

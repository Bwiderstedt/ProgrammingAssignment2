###############################################################################
#	This script contains functions to create a special kind of matrix 
#	cachedMatrix, that has methods to get and set matrix values and 
#	the matrix inverse. 
#	
#	Methods for a cachedMatrix CM 
#		CM$set(x)
#			@x = a square, invertable matrix 
#			@return 
#				x = the matrix values 
#				invertedMatrix = NULL 
#		CM$get()
#			@return = the matrix values 
#		CM$setInverse(y)
#			@y = the inverse of the matrix (x)
#			@return = the inverse of the matrix (x)
#		CM$getInverse()
#			@return = the inverse of the matrix (x) 
#
#	When created, the cachedMatrix has no inverse calculated, 
#	To calculate inverted matrix, call cacheSolve(CM) 
###############################################################################


###############################################################################	
#	makeCacheMatrix 
#		 Creates a matrix that can store and cache values 
#		@args 
#			x = a matrix to store as cachedMatrix (assume square and invertable) 
#		@return 
#			- a list of functions / methods for the cachedMatrix 
###############################################################################
makeCacheMatrix <- function(x = matrix()) {

	invertedMatrix <- NULL 
# method to assign the matrix to be inverted to (cached) x - 
	setMatrix <- function (y) {
	          x <<- y
              invertedMatrix <<- NULL
	}
# method to return the matrix data 
	getMatrix <- function() {
		x 
	}
# method to set the inverse of matrix ** DOES NOT CALCULATE INVERSE **
	setInverse <- function(inverseMatrix) {
		invertedMatrix <<- inverseMatrix 
	}
# metod to return the inverse of the matrix x 
	getInverse <- function() {
		invertedMatrix
	}
# returns the list of methods available to active matrix 
	list (setMatrix = setMatrix, getMatrix = getMatrix, 
			setInverse=setInverse, getInverse=getInverse) 
}

###############################################################################
# cacheSolve 
#			returns the inverse of a cachedMatrix. 
#			If inverse is already calculated, then return the cached version
#			If inverse is not calculated, then calucalate inverse and store 
#				in cachedMatrix 
#		@args
#			x = the cachedMatrix to invert 
#		@return
#			- the inverse of the @args cached Matrix 
# #############################################################################

cacheSolve <- function(x, ...) {
 	# try getting inverse 
	inverseMatrix <- x$getInverse() 
# if inverse already exists, return cached inverse 
	if(!is.null(inverseMatrix)) {
		message ("Returning cached matrix inverse") 
		return(inverseMatrix) 			
	} 							
# calculate inverse since not exists 
	matrixData <- x$getMatrix() 				# get the matrix to invert 
	inverseMatrix <- solve(matrixData)			#Invert the matrix 

	invertedMatrix <- x$setInverse(inverseMatrix) 		# save into cachedMatrix 
#return inverted matrix 
	inverseMatrix
}

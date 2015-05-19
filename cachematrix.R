## R Programming - Assigment #2
## References: Caching the Mean of a Vector example exercise
## Two functions (makeCacheMatrix, and cacheSolve) to cache matrix inverse; and return cached values



## ---------------------------------------------------------------------------------------------
## makeCacheMatrix fuction creates a list of functions to perform operations on a given matrix x 
## Functions include: getmatrix - return matrix x
##			    setmatrix - set matrix to new matrix "newmatrix"
##			    setinv - calculate inverse of matrix x
## 			    getinv - return cached value of matrix x
## To declare new object use command "variable" <- makeCacheMatrix("some matrix here")
## To reference object and access functions use commands "variable"$"somefunction"("any parameters")
## Dependencies/Assumptions: x and "newmatrix" must be an invertable matrix
## ----------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

	# declare empty matrix to store inverse
	
	# R library reference says matrix should be NA for atomic vectors, and NULL for lists, will use NA.
	# reference: "http://127.0.0.1:14975/library/base/html/matrix.html"
	# last accessed: 5/19/2015

	# code to define inv matrix as null
	# reference: stackoverflow article, "http://stackoverflow.com/questions/1745622/best-way-to-allocate-matrix-in-r-null-vs-na" 
	# last accessed: 5/18/2015 

	# define NULL matrix (not used)
	# lenmatrix<-dim(x)[1]
	# matrixinv <-list()
	# length(matrixinv)<-lenmatrix^2
	# dim(matrixinv)<-c(lenmatrix,lenmatrix)

	# define NA matrix
	matrixinv <- matrix(data=NA,nrow=lenmatrix,ncol=lenmatrix)

	# used the ... argument to make it so optional arguments wouldn't break with error
	setmatrix <- function(newmatrix, ...) {
		# redefine existing matrix and set inv to NA's
		x<<-as.matrix(newmatrix)
		matrixinv <- matrix(data=NA,nrow=lenmatrix,ncol=lenmatrix)
	}

	getmatrix <- function(...) x

	setinv <- function(...) {
		matrixinv <<- solve(x)
	}

	getinv <-function(...) matrixinv

	#return list of available functions for object
	list(setmatrix=setmatrix, getmatrix=getmatrix, setinv=setinv,getinv=getinv)
}

## ---------------------------------------------------------------------------------------------------------
## cacheSolve checks to see if matrix inverse exists; if so it returns cached values, else it calculates inverse and sets value using object from makeCacheMatrix.
## To use, try command cacheSolve("variable") where "variable" is the object you assigned to makeCacheMatrix("some matrix here") from above function
## To see it use return cached values, run cacheSolve command twice or use "variable"$setinv(), and then cacheSolve("variable")
## Dependencies/Assumptions: must have object "variable" created using makeCacheMatrix first
## --------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#get matrixinv of object
	matrixinv <-x$getinv()

	#if all elements in the matrixinv are NA, get solve for inverse
	# c(t(somematrix)) is one method to vectorize a matrix; once in vector form can use 'all' to check if they are all NA
	if(!all(is.na(c(t(matrixinv))))) {
		message("getting cached data")
            return(matrixinv)
        }
	#else:
	matrixdata <-x$getmatrix()
	matrixinv <- solve(matrixdata)
	x$setinv()
	matrixinv

}

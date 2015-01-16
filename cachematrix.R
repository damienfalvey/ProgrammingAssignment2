# makeCacheMatrix is a function that returns a list of functions. Its puspose is to store a martix and a cached value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}

	get<-function() x
   ## Declare Getters & Setters for Matrix
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
   ## Assign Getters & Setters in List
	list(set=set, get=get,
		setmatrix=setmatrix, ## Assigns the stored matrix
		getmatrix=getmatrix) ## returns the stored matrix
}


# The following function calculates the inverse of a "special" matrix created with makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
   ## get the cached value
	m<-x$getmatrix()
   ## If a cached value exists return it
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

   ## otherwise get the matrix, caclulate the inverse and store it in the cache
	matrix<-x$get()
	m<-solve(matrix, ...)
	x$setmatrix(m)
	m
}

## --- Test ---

a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
cacheSolve(a)

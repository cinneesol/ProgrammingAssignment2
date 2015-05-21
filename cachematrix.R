#### Coursera           : Data Science 
####                    : R Programming 
####                    : Assignment 2

# This second programming assignment will require you to write an R function is able to cache potentially 
# time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. 
# However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed 
# repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of 
# the mean so that when we need it again, it can be looked up in the cache rather than recomputed. 
# In this Programming Assignment will take advantage of the scoping rules of the R language and how they can be 
# manipulated to preserve state inside of an R object.

# The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to
# 1.) set the value of the matrix
# 2.) get the value of the matrix
# 3.) set the value of the inverse
# 4.) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                # initialise the cached inverse to nothing as a default
        
        # Caching a new input matrix 
        set <- function(y) {
                # The <<- operator can be used to assign a value to an object in an environment that is different 
                #from the current environment. 
                
                x <<- y         # Cache the new input matrix
                m <<- NULL      # and initialise the cached inverse to nothing, as it has not been calculated
        }
        
        # getting the cached input matrix x
        get <- function() x
        
        # Setting and getting the cached inverse
        setInverse <- function(invrse) m <<- invrse
        getInverse <- function() m
        
        # return the list with the input and inverse matrix set/get functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse 
# in the cache via the setInverse function.
#
# NOTE: 
#       1.) we assume that the matrix is always invertible.
#       2.) we assume x is a makeCacheMatrix object


cacheSolve <- function(x, ...) {
        
        # First let's see if the inverse has been calculated. If so, return the cached inverse 
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if not, get the cached input matrix
        data <- x$get()
        
        # and calculate the inverse
        #
        # For the square matrix A, the inverse X is gotten by solving AX=B, where B is the identity matrix.
        # (The identity matrix is a square matrix with the same dimentions as A. It has ones on the diagonal
        # and zeros everywhere else.)
        
        m <- solve(a = data, b = diag(dim(data)[1]),...)
        
        # Now set the cached inverse
        x$setInverse(m)
        
        # and also return the inverse result
        m
        
}


##### Test The functions
if(TESTFRAMEWORK)
{
        # Setup an invertible  matrix
        A <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3)
        
        #make a cached matrix
        Acached <- makeCacheMatrix(x=A)
        invse<-cacheSolve(Acached)
        cachedInvse<-cacheSolve(Acached)
        if(!identical(invse,cachedInvse)) stop(" The cached inverse and calculated inverse is not the same")
        
        # Set new data
        A <- matrix(c(1,0,1,2,4,0,3,5,6),nrow=3)
        Acached$set(A)
        Aget<-Acached$get()
        if(!identical(A,Aget)) stop(" The cached input matrix is not the same as the original")
        
        invse<-cacheSolve(Acached)
        cachedInvse<-cacheSolve(Acached)        
        if(!identical(invse,cachedInvse)) stop(" The cached inverse and calculated inverse is not the same")
        
}

## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix(x) creates a vector of functions that share common environ-
## ment and in it the matrix x and the inverse of it "solved" these functions
## can then be used to get or store the matrix and its inverse.
## function set can be used to change the stored matrix (it deletes the cached 
## inverse in the process)
## function cacheSolve makes use of this vector to 1. find out if there is al-
## ready a cached inverse present. if yes, it is returned, if not it is computed
## and stored. 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    get <- function() x
    # allows setting a new matrix (deletes the cached version) 
    set <- function(matrix){
        x<<- matrix
        message("deleting cached data")    
        solved<<- NULL
        } 
    # set solved allows us to save inversed matrix when computed
    setSolved <- function(solvedMatrix) solved <<- solvedMatrix
    
    getSolved <- function() solved
    list(get = get,set=set,
         setSolved = setSolved,
         getSolved = getSolved)
}


## Write a short comment describing this function
## function cacheSolve makes use of this vector to 1. find out if there is al-
## ready a cached inverse present. if yes, it is returned, if not it is computed
## and stored.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x' 
        
        #check if matrix was already solved
        solved <- x$getSolved()
        if(!is.null(solved)) {
            #if so, tell the user
            message("getting cached data")
            # and return stored inverse
            return(solved)
        }
        #if no cached verison, get the matrix, invert it, save and return
        data <- x$get()
        solved <- solve(data)
        x$setSolved(solved)
        solved
    }

#test case (must use solvable matrix)
CacheMatrix<-makeCacheMatrix(matrix(c(1,2,10,2,10,2,1,2,1,1,
                                      2,10,2,10,2,1,2,11,2,10,
                                      2,10,2,1,2,11,2,10,2,10,
                                      2,1,2,1,1,1),
                                      6,6))

# first run 
cacheSolve(CacheMatrix)
# second run
cacheSolve(CacheMatrix)
# store different matrix

CacheMatrix$set(cacheSolve(CacheMatrix))
# first run 
cacheSolve(CacheMatrix)
# second run
cacheSolve(CacheMatrix)

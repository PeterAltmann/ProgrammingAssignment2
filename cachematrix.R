# The two functions below work together in order to calculate the inverse of
# a matrix. Once an inverse has been calculated, the inverse is stored in cache.
# As lond as the matrix does not change, any request for the matrix inverse will
# be supplied from cache (much faster than having to run solve() again).

# makeCacheMatrix takes a matrix (squared with existing inverse) as an input.
# Then it creates a list with four functions, that can be used to get and set 
# values for the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #Makes sure any cached value is cleared with new matrix input
        set <- function(y) { #the function that allows x$set() to be used
                x <<- y #changes matrix x to matrix y in parent env.
                i <<- NULL #clears cache in parent env.
        }
        get <- function() x #this enables x$get() to retrieve the matrix
        setinverse <- function(inverse) i <<- inverse #cacheSolve sets i <<- i
        getinverse <- function() i #returns the inverse if exists, NULL if not.
        list(set = set, get = get, #this creates a list with the four functions
             setinverse = setinverse, 
             getinverse = getinverse)
}

# cacheSolve is the actual function that calculates the inverse.
cacheSolve <- function(x, ...) {
        i <- x$getinverse() #function first checks if there exists cached value
        if(!is.null(i)) { #if cached value exists, then executes function
                message("Getting cached matrix inverse") #simple msg.
                return(i) #returns the inverse from cache
        }
        data <- x$get() #if the cache is NULL, it retrieves matrix using $get()
        i <- solve(data, ...) #calculates inverse of matrix, stores value as i
        x$setinverse(i) #stores the inverse in cache
        i #returns the inverse
}
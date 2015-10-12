## makeCacheMatrix, cacheSolve - caching system for matrices

## initialize with matrix data as:
## my_matrix <- makeCacheMatrix(matrix(rnorm(25),5,5)) # square matrix of 5-on-5 filled by generated normal data
## cacheSolve(my_matrix) # computes inverse matrix and caches data
## cacheSolve(my_matrix) # writes cached matrix, does not compute anything
## my_matrix$set(matrix(rnorm(25),5,5)) # fill object with new matrix

## cacheble matrix function
## x: initializing matrix
## return: list of available actions

makeCacheMatrix <- function(x = matrix()) {
    
    # init solution value
    sol <- NULL
    
    # set matrix after first initialization
    set <- function(y) {
        x <<- y
        
        # clear cached value
        sol <<- NULL
    }
    
    # get original matrix
    get <- function() x
    
    # set cached value
    setsolve <- function(solve) sol <<- solve
    
    # get cached value
    getsolve <- function() sol
    
    # return a list of actions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## inversion matrix solving with cashing usable with makeCacheMatrix function
## x makeCacheMatrix function
## ... solve() additional arguments
## return: inversed matrix, cached or computed

cacheSolve <- function(x, ...) {
    # fetching for possibly computed value of inverted matrix
    sol <- x$getsolve()
    if(!is.null(sol)) {
        message("getting cached data")
        # returning cached inverted matrix, function terminates
        return(sol)
    }
    
    # computing inverted matrix, no cache data available
    data <- x$get()
    
    # performing solve() on matrix
    sol <- solve(data, ...)
    
    # caching the solution to makeCacheMatrix
    x$setsolve(sol)
    
    #returning computed data
    sol
}

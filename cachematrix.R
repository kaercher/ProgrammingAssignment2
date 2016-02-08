## Both function work together to retorn the inverse of a matrix x
## to save time in instead of calculating the inverse two times if the inverse was
## already calculated the saved inversed matrix is returned

## This fist function creates a matrix and functions to modify this matrix and save a
## copy of its inverse when it has been already calculated

makeCacheMatrix <- function(x = matrix()) {
    inv_cache <- NULL
    set_Matrix <- function(y){
        x <<- y
        inv_cache <<- NULL
    }
    get_Matrix <- function() x
    get_inverted_Matrix <- function() inv_cache
    set_inverted_Matrix <- function(result) inv_cache <<- result
    list(set_Matrix = set_Matrix,
         get_Matrix = get_Matrix ,
         get_inverted_Matrix = get_inverted_Matrix,
         set_inverted_Matrix = set_inverted_Matrix)
}


## This second functon calculates the inverse of the matrix if this has not been done already 
## and gets the saved inverted matrix if it has been already calculated

cacheSolve <- function(x, ...) {
    m <- x$get_inverted_Matrix()
    if(!is.null(m)) {
        message("getting cached inverted matrix")
        return(m)
    }
    data <- x$get_Matrix()
    m <- solve(data, ...)
    x$set_inverted_Matrix(m)
    m
}

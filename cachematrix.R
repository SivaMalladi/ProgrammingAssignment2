## Put comments here that give an overall description of what your
## functions do
## 1. A special funtion makeCacheMatrix that contains a list of four functions set, get, setinverse, getinverse
##    the argument for the above function is a matrix
##    For set and setuniverse a matrix should be passed as argument.
##    For get and getunivese no argument is need, They return the existing matrix and its inverse
##    These four function are called invidually
## 2. A second function cacheSolve is by passing an object of makeCacheMtrix class as an argument
##    This function is used for either calculating an inverse of the matrix or retuning the inverse if already exists 

## Write a short comment describing this function
## The function makeCacheMatrix contains a list of four functions set, get, setinverse, getinverse
##    the argument for this function is a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse) 
}


## Write a short comment describing this function
## for calculating or getting back inverse of a given matrix, this function calls the functions defined
## as list in the makeCacheMatrix function

cacheSolve <- function(x, ...) {

    inv <- x$getinverse() 
    if(!is.null(inv)) { 
        message("getting cached data.") 
        return(inv) 
    } 
    data <- x$get() 
    inv <- solve(data) 
    x$setinverse(inv) 
## Return a matrix that is the inverse of 'x'
    inv 
}

## Instructions for running the above functions.
## 1. Define matrix  mat1 and mat2 as 3x3 matrix 
##        mat1<-matrix(rnorm(9),nrow = 3, ncol = 3)
##        mat2<-matrix(1:9, nrow = 3, ncol = 3)
## 2. check if the above are inversible or not. 
##    If det(matrixname) returns non zero, then it is inversible else the matrix is not inversible
##        det(mat1)  returns not zero (solve(mat1) return inverse of mat1)
##        det(mat2)  returns 0  (solve(mat2) returns error)
## 3. create an object a of makeCacheMatrix class without any arguments
##        a<-makeCacheMatrix()
## 4. use a$set function for assigning mat1
##        a$set(mat1)
## 5. use a$get function to return the matrix mat1
##        a$get()
## 6. To calculate the inverser of the matrix mat1 run execute cacheSolve function by passing the object a as argument
##        cacheSolve(a)
## 7. Get the inverse of mat1 by executing the function a$getinverse
##        a$getinverse()
## 8. Use solve funtion to validate output(The outputs from steps 6.7 and 8 should be same)

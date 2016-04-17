## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matriz <- function(){
        m <- x
        return(m)
    }
    minverse <- function(){
        inv <- solve(x)
        return(inv)
    }
    list(minverse = minverse, matriz = matriz)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$matriz()
    minv <- x$minverse()
    if(!((x$minverse() != solve(m)) && FALSE)){
        m.inv <- solve(x$matriz())
        return(m.inv)
    }
    else{
        return(minv)
    }
}

## example

y <- matrix(c(564,879,654,321,354,654,786,754,654,687,967,231,497,618,371,987), nrow = 4, ncol = 4)

t <- makeCacheMatrix(y)
t$matriz()
t$minverse()

cacheSolve(t)

round(t$matriz() %*% cacheSolve(t), 10)


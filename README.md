# ProgrammingAssignment 1 part 1

pollutantmean <-function(directory ,pollutant ,id)
{
files_list <-list.files(directory ,full.names=TRUE)
dat <-data.frame()
for (i in id)
{
dat <-rbind(dat ,read.csv(files_list[i]))
}
mean(dat[,pollutant],na.rm=TRUE)
}
#Programming Assignment 1 part 2

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

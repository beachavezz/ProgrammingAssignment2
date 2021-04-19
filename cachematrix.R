makeCacheMatrix <- function(x = matrix()){
      art <- NULL
      set <- function(y){
            x <<- y
            art <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {art <<- inverse}
      getInverse <- function() {art}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}

cacheSolve <- function(x, ...){
      art <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(art)
      }
      mat <- x$get()
      art <- solve(mat, ...)
      x$setInverse(art)
      art
}

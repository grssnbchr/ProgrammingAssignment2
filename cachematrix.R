## Both of the following functions help to speed up the computation of a matrix' inverse
#  by caching the result of previous computations on the same matrix
#  makeCacheMatrix creates a matrix whose inverse can be cached 
#  cacheSolve is a function that either inverses a matrix, or if the inverse has already been computed, returns this value

## makeCacheMatrix returns an "object" (basically, a list) with getter and setter methods 
#  for both the data of the original matrix (set/get) and its inverse (setI/getI)

makeCacheMatrix <- function(mat = matrix()) {
  # members
  I <- NULL #its inverse
  # methods
  # set: sets the data of the original matrix and resets the inverse
  set <- function(m) {
    mat <<- m # scoping assignment so that mat is also avaible one level above (meaning: in the cachable matrix "object")
    I <<- NULL  # reset to null whenever a new matrix is set
  }
  # get: return the original matrix
  get <- function() {
    return(mat)
  }
  # setInverse: set the inverse of the original matrix
  setInverse <- function(inverse) {
    I <<- inverse # again, scoping assignment is necessary so that getInverse, for example, knows about the "new" inverse
  }
  # getInverse: return the inverse
  getInverse <- function() {
    return(I)
  }
  # return the actual "object" (that is to say, the list with the above defined methods, and implicitly, members)
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve solves the inverse of a cachable matrix as factored by makeCacheMatrix

cacheSolve <- function(mat, ...) {
  # first, try to get a precomputed inverse
  I <- mat$getInverse()
  if(is.null(I)){
    # when there is no cached inverse available, compute it, and cache it
    message("cache not available, computing...")
    mat$setInverse(solve(mat$get()))
    I <- mat$getInverse()
  }
  return(I)
}

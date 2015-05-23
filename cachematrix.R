## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  envCache<-NULL
  set<-function(y)
     {
        <<-y
        envCache<<-NULL
     }
  get<-function() x
  
  setmatrix<-function(solve) envCache<<- solve
  
  getmatrix<-function() envCache
  
  list(set=set, get=get,   setmatrix=setmatrix,   getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        envCache<-x$getmatrix()
        if(!is.null(envCache))
        {
                message("getting cached data")
                return(envCache)
        }
        matrix<-x$get()
        envCache<-solve(matrix, ...)
        x$setmatrix(envCache)
        envCache
}

## This function series will allow for cache the inverse of a matrix to ave having to recompute the inverse
## at future points

## This function takes the input opf a matrix and passes along other information in a list in order to be able to cahce
## the inverse

makeCacheMatrix <- function(x = matrix()) {    ##creating "matirx" list
                                          
  
  M<- NULL                        ## setting intial value of M to NULL
  
  
  set <-function(y){
    x<<- y           ##'<<-' causing search through parent environment to see if already set
    M <<- NULL       ##'<<-' causing search through parent environment to see if already set
  }
  get<- function() x
  setinverse <- function (solve) M<<-solve     ##storing inverse solution
  getinverse <-function() M
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)    ##outputing completed list
}



## This function takes a specailly prepared matirx and eitehr calculates the inverse or uses the cache to 
## retrieve a previosuly calculted inverse

cacheSolve <- function(x, ...) {   ##passing list from created makCacheMatrix matrix to function
  
  M<-x$getinverse()       ##reading inverse matrix from 
  if(!is.null(M)){
    message("getting cached data")  ##if there is cached inversed matix, informs user
    #function is grabbing cahced version
    
    return(M)                       ##grabbing cached inverese
  }
  
  data<- x$get()                          ##pulling data from orginal matrix
  M <-solve(data,...)                     ##finding inverse if it doesn't already exist
  x$setinverse(M)                         ##assinging M to the setmatrix (sovle) in passed list
  M                                       ## Return a matrix that is the inverse of 'x'
}


## makeCacheMatrix creates a list of functions, as described below.
## Ecah of these functions may be called by cacheSolve to return the matrix and its inverse, as required.

## makeCacheMatrix sets the glabal values of the inv and x variables.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL                                   #Every time you run makeCacheMatrix inv is reset to NULL
    set<-function(y){       
        x<<-y
        inv<<-NULL
    }
    get<-function() x                           #This just returns the value of x, the list of functions.
                                                #This will be use in the call to the solve function when it is called in the cacheSolve function.
    setinv<-function(inverse) inv <<- inverse   #Assigns the inverted matrix to the variable inv (i.e. creates the cache)
    getinv<-function() inv                      #Returns the value of inv, whether inverted or NULL
    list(set=set,get=get,                    #Creates the list of four functions
         setinv=setinv,getinv=getinv)    
}


## Uses the functions in makeCacheMatrix to generate the inverse, or return it if already cached.

cacheSolve <- function(x, ...) {                #Argument to cacheSolve is output of makeCacheMatrix
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()                             #Calls getinv which returns the iverted matrix.
    if(!is.null(inv)){                          #Checks that inv isn't NULL. If it isn't, then the cached data is accessed by returning the value of inv.
        message("getting cached inverted matrix data")
        return(inv)
    }
    data<-x$get()                               #Returns the list of functions
    inv<-solve(data, ...)                       #Assigns the inverted matrix to the variable inv. This is where the inversion is carried out, if necessary.
    x$setinv(inv)                               #Calls the setinv function in mackeCacheMatrix (i.e. creates the cache)
    inv                                         #Returns the inverted matrix
}
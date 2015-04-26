### 2 Functions to cache the inverse of matrix
### Create matrix which is a list of functions which compute inverses of matrix
makeCacheMatrix<- function (x=numeric()) {
   m<- NULL
   set <-function (y) {  ### change the matrix-Sub x with y and null it
     x<<-y
     m<<-NULL
   }
   get <- function()x    ### return x 
   setsolve <- function(solve) m <<-solve  ### store value of solve
   getsolve <- function () m               ### return into getsolve
   list (set=set, get=get,   ### create a list which has all 4 functions
         setsolve=setsolve,
         getsolve=getsolve)
}
### Function to compute the inverse of a matrix from makeCacheMatrix
cachesolve <- function(x) {
   m <- x$getsolve()      ### check m from setsolve above for empty
   if (!is.null(m)) {
       message("getting cache data")
       return(m)
   }
   data<- x$get()         ### return cachematrix
   m<- solve(data)        ### return inverse of a matrix
   x$setsolve(m)          ### store it back into cachematrix
   m
}


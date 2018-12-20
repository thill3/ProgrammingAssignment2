## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #This function creates a special "matrix" object that can cache its inverse.
        inverse <- NULL #Ckear the "inverse" variable
        set <- function(y) { #create a function "set" that takes an argument y
                x <<- y #set x (which is inside of the scope of this internal 
                #function) to have the value of y (which is an argument to
                #the set function)
                #This will allow for using the $set functionality
                inverse <<- NULL
        }
        get <- function() x #"get" is a function that takes no arguments and
                #returns x
        setInverse <- function(matrix) { inverse <<- solve(x) }
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) #The makeCacheMatrix function will return a list
        #of set, get, setinverse, and getinverse - all functions
        #basically, it's storing a vector and that vector's inverse
        #after building some functions it puts those functions in a list
        #So there's an object with 4 methods and 2 attributes
        #Naming the list elements is what allows us to use the $ form of
        #the extract operator to access the functions by name 
        #rather than using the [[ form of the extract operator, 
        #as in myVector[[2]](), to get the contents of the 
        #vector.
}


## I tend to comment my code heavily. Helps me keep track of what's going on.
        #The above function creates the matrix object and its methods 
        #(functions) 
        #set will give it a new matric.
        #get retrieves the input matrix
        #setInverse allows for setting an inverse based ona  new matric
        #getInverse allows for getting the inverse
        #the list command allows us to use $ to access the internal functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #This function computes the inverse of the special "matrix" returned by 
        # makeCacheMatrix above. If the inverse has already been calculated (and
        # the matrix has not changed), then cacheSolve should retrieve the 
        # inverse from the cache.
        #cachemean is a function that takes an argument 
        #x and whatever other arguments
        inverse <- x$getInverse() #This command will call getInverse() to get 
                #the inverse from the makeCacheMatrix function. If it's already 
                #there then this gets the saved value. If it's not then this 
                #returns a NULL.
        if(!is.null(inverse)) { #if inverse variable is not NULL, that is, if 
                        #there is a cached matrix
                message("getting cached data")
                return(inverse) #return the cached inverse
                #Note that if inverse is NULL then this 'if' block doesn't get run and 
                #inverse is not retrieved
                #Because there's a return here we only run the rest of the code
                #if there is no 'inverse' value (that is, if there's nothing in 
                #the cache).
        }
        data <- x$get() #retrieve the matrix x that went into this
        #function and store it in "data"
        m <- solve(data, ...) #take the inverse of the data matrix. Store in 
                #the variable m
        x$setInverse(m) #set the inverse value on x to be m
        m #return m
}


#testing examples
aMatrix <- makeCacheMatrix(x = matrix(c(1,1,2,1), 2,2))
aMatrix$getInverse()
cacheSolve(aMatrix)
cacheSolve(aMatrix)

aMatrix <- makeCacheMatrix(x = matrix(c(2,1,1,1), 2,2))
aMatrix$getInverse()
cacheSolve(aMatrix)
cacheSolve(aMatrix)

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object) #should give n1, which it does. Appropriately 
        #it calculated rather than retrieved.
cacheSolve(myMatrix_object) #Now it is retrieved rather than recalculated






makeVector <- function(x = numeric()) { #input a vector x
        m <- NULL #initialize variable m to NULL. This will clear any value that
                #was already there.
        set <- function(y) { #create a function "set" that takes an argument y
                x <<- y #set x (which is inside of the scope of this internal 
                #function) to have the value of y (which is what is called by
                #the set function)
                m <<- NULL #set m (outside of the scope of this internal function) 
                #to have the value of NULL (didn't we do that already?)
        }
        get <- function() x #"get" is a function that takes no arguments and
                #returns x from earlier
        setmean <- function(mean) { m <<- mean } #Since m is defined in the parent 
        #environment and we need to access it after setmean() completes, the 
        #code uses the <<- form of the assignment operator to assign the input 
        #argument to the value of m in the parent environment.
        getmean <- function() m #getmean is a function that takes no arguments 
                #and returns m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean) #The makevector function will return a list
                #of set, get, setmean, and getmean - all functions
                #basically, it's storing a vector and that vector's mean
                #after building some functions it puts those functions in a list
                        #So there's an object with 4 methods and 2 attributes
                #Naming the list elements is what allows us to use the $ form of
                        #the extract operator to access the functions by name 
                        #rather than using the [[ form of the extract operator, 
                        #as in myVector[[2]](), to get the contents of the 
                        #vector.
}

cachemean <- function(x, ...) { #cachemean is a function that takes an argument 
        #x and whatever other arguments
        m <- x$getmean() #This command will call getmean() to get the m from 
                #the makevector function. If it's already there then this gets 
                #the saved value. If it's not then this returns a NULL.
        if(!is.null(m)) { #if m is not NULL, that is, if there is a cached mean
                message("getting cached data") #show a message that the function
                        #is retrieving the cached item
                return(m) #and return the m that we just got
                #Note that if m is NULL then this if block doens't get run and 
                        #m is not retrieved 
        }
        data <- x$get() #retrieve the vector x that went into the previous
                #function and store it in "data"
        m <- mean(data, ...) #take the mean of data and anything else. Store in
                #the variable m
        x$setmean(m) #set the mean value on x to be m
        m #return m
}


#one <- function(x) 1
#two <- function(y) {2}

#one(3) #1
#two(4) #2
#so conclude that for a one line function the "{}" are not needed

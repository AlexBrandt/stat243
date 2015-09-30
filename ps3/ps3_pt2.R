# While I didn't work with any other students on this question, I found
# the S4 classes a little complicated, and consulted print/web sources
# pretty extensively.  As always, I attempted to do so in a way that 
# forced my own understanding (no copy + paste), but these were great
# resources that I didn't want to omit from my write-up.
#
# Citations: 
# https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf
# http://www.cyclismo.org/tutorial/R/s4Classes.html
# http://practicalcomputing.org/node/80
#
# We begin by defining rw, which holds our various slots
rw <- setClass(
  "rw",
  # The basic slots associated with our class
  slots = c(
    start = "numeric",
    steps   = "numeric",
    trajectory_recording = "logical",
    .trajectory = "matrix"),
  
  # Now we declare our default values
  prototype=list(
    start = c(0,0),
    #steps = 10,
    trajectory_recording = TRUE
  ),
  # Look for things that might be amiss
  validity=function(object)
  {
    # We need our steps to be greater than 0...
    if(object@steps<1) {
      return("Please enter 
             a positive number of steps.")
    }
    # and integers...
    if(as.integer(object@steps)!=object@steps) {
      return("Please enter 
             an integer valued number of steps.")
    }
    # and we should only be working in 2D.
    if(length(object@start)!=2) {
      return("This program is
             only written for 2D (for now!).")
    }
    return(TRUE)
  }
)

# Define the method to replace our start position
setGeneric("start<-", function(self, value) standardGeneric("start<-"))

setReplaceMethod("start",
  "rw",
  function(self,value) {
  self@start <- value
  self
  }
)

# Define the rw[i th] <- replacement method
setMethod(
  f="[",
  signature="rw",
  definition=function(x,i,drop){
    mypath=slot(x,".trajectory");
    xs=sum(mypath[1:i,1]);
    ys=sum(mypath[1:i,2]);
    return(c(x@start[1]+xs, x@start[2]+ys))
  }
)

# Define our plotting function
setMethod(
  f="plot",
  signature="rw",
  definition=function(x){
    mypath=slot(x,".trajectory");
    # here we use "cumsum" to get the cumulitive
    # displacement vector at each point in time
    # which we add to our start
    xs=cumsum(mypath[,1]);
    ys=cumsum(mypath[,2]);
    plot(x@start[2]+ys,x@start[1]+xs, type='o', 
         xlab='x-position', ylab='y-position',
         main='A Random Walk Trajectory');
  }
)

# Some details about the walk.  We will print out
# the entire trajectory if the user has specificed
# that they would like to with the optional argument
setMethod(
  f="print",
  signature="rw",
  definition=function(x){
    print("Starting position:")
    print(slot(x,"start"))
    print("After this many steps...:")
    print(slot(x,"steps"))
    print("We arrive at:")
    print(x[slot(x,"steps")])
    if (slot(x,"trajectory_recording"))
    { print("List of positions visited in order:")
      for (i in 1:slot(x,"steps")) {
        print(x[i]) }}
  }
)

# The simulate method initializes the walk
setGeneric("simulate", 
           function(x){standardGeneric("simulate")})

setMethod(
  f="simulate",
  signature="rw",
  definition=function(x){
    # Set our random seed for reproduceable results.
    set.seed(0)
    slot(x,".trajectory") <- 
      matrix(c(0, 1, -1, 0, 1, 0, 0, -1), 
             nrow=4, 
             ncol=2)[sample(4,size=slot(x,"steps"),
             replace=TRUE),];
    return(x)
  }
)

# Testing the replacement class
my_walk <- new("rw", start=c(1,1),steps=1000,trajectory_recording=FALSE)
# We initialize the walk with the simulate() method -- I don't love this.
# But to circumvent, I would need to use the "assign" function, which
# the S4 manual cautions against! 
# (https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf)
my_walk <- simulate(my_walk)
# Show the 50th position
my_walk[50]
# Plot before we move the start
plot(my_walk)
# Change the start position
start(my_walk)<-c(100,100)
# Plot AFTER we move the start
# ...Hopefully they are the same shape, just shifted!
plot(my_walk)
# print the summary statistics
print(my_walk)

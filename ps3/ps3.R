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
    # REMEMBER TO ADD INTEGER CHECKS.
    if(object@steps<0) {
      return("Please enter 
             a positive number of steps.")
    }
    if(length(object@start)!=2) {
      return("This program is
             only written for 2D (for now!).")
    }
    return(TRUE)
  }
)


# Found "OOP in R" (http://practicalcomputing.org/node/80) to be very useful.
setGeneric("start<-", function(self, value) standardGeneric("start<-"))

setReplaceMethod("start",
  "rw",
  function(self,value) {
  self@start <- value
  self
  }
)

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

setMethod(
  f="plot",
  signature="rw",
  definition=function(x){
    mypath=slot(x,".trajectory");
    xs=cumsum(mypath[,1]);
    ys=cumsum(mypath[,2]);
    plot(x@start[2]+ys,x@start[1]+xs, type='o');
  }
)

setGeneric("simulate", 
           function(.Object){standardGeneric("simulate")})

setMethod(
  f="simulate",
  signature="rw",
  definition=function(.Object){
    slot(.Object,".trajectory") <- 
      matrix(c(0, 1, -1, 0, 1, 0, 0, -1), 
             nrow=4, 
             ncol=2)[sample(4,size=slot(.Object,"steps"),
             replace=TRUE),];
    return(.Object)
  }
)

# Testing the replacement class
my_walk <- new("rw", start=c(1,1),steps=1000)
# To circumvent this, I would need to use the "assign" function, which
# the S4 manual cautions against! 
# (https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf)
my_walk <- simulate(my_walk)
my_walk[50]
plot(my_walk)
start(my_walk)<-c(100,100)
plot(my_walk)

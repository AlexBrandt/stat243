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
    steps = 10,
    trajectory_recording = TRUE#,
    #.trajectory = matrix(c(0, 1, -1, 0, 1, 0, 0, -1), 
    #       nrow=4, 
    #       ncol=2)[sample(4,size=10,replace=TRUE),]
  ),
  # Look for things that might be amiss
  validity=function(object)
  {
    if(object@steps<0) {
      return("Please enter a positive number of steps.")
    }
    if(length(object@start)!=2) {
      return("This program is only written for 2D (for now!).")
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
    return(slot(x,".trajectory")[i,])
  }
)

setMethod(
  f="plot",
  signature="rw",
  definition=function(x){
    mypath=slot(x,".trajectory");
    print(mypath[,1]);
    xs=cumsum(mypath[,1]);
    ys=cumsum(mypath[,2]);
    plot(x@start[1]+xs, x@start[2]+ys, type='o');
  }
)
length(c(1,1))==2
# Testing the replacement class
alex <- new("rw", start=c(1,1),steps=10,trajectory_recording=TRUE) 
start(alex)<-c(4,4)
plot(alex)

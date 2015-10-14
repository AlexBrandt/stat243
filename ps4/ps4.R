# Problem 1)

set.seed(0)
runif(1)

save(.Random.seed, file = 'tmp.Rda')
runif(1)

load('tmp.Rda')
runif(1)

tmp <- function() {
  # Added globalenv()
  load('tmp.Rda',envir = globalenv())
  runif(1)
}
tmp()

# Problem 2)

denominator <- function(k,n,p,phi) {
  a <- lchoose(n,k);
  b <- k*log(k) + (n-k)*log(n-k) - n*log(n)
  c <- phi * (n*log(n) - k*log(k) - ((n-k)*log(n-k)));
  d <- (k*phi)*log(p);
  e <- (n-k)*phi*log(1-p);
  return(exp(a + b + c + d + e))
}

# The "corner cases" where k=0 or k=n.
denominator_cc <- function(k,n,p,phi) {
  a <- lchoose(n,k);
  b <- log(1)
  c <- phi * log(1);
  d <- (k*phi)*log(p);
  e <- (n-k)*phi*log(1-p);
  return(exp(a + b + c + d + e))
}

N <- 2000
a <- sequence(N-1)

# 2a)
ptm <- proc.time()
v1 <- unlist(lapply(a,denominator,n=N,p=0.3,phi=0.5))
sum(v1) + denominator_cc(k=0,n=N,p=0.3,phi=0.5) + 
  denominator_cc(k=N,n=N,p=0.3,phi=0.5)
proc.time() - ptm

# 2b) FULLY VECTORIZED!
ptm <- proc.time()
v2 <- denominator(a,n=N,p=0.3,phi=0.5)
sum(v2) + denominator_cc(k=0,n=N,p=0.3,phi=0.5) + 
  denominator_cc(k=N,n=N,p=0.3,phi=0.5)
proc.time() - ptm


# Question 3

# Import the matrix library that allows for a sparse matrix.
# Otherwise, it would overwhelm even powerful RAM systems.

library(Matrix)
mmrda <- "~/Dropbox/solutions_github/stat243/ps4/mixedMember.Rda"
load(mmrda)

# 3a)

# In the "one line" sapply function, the weights are applied
# to the mu's given by a set of id's in the IDsX object.

my_sums_A <- sapply(1:length(IDsA), 
                    function(i) sum(wgtsA[[i]] * muA[IDsA[[i]]]))
my_sums_B <- sapply(1:length(IDsB), 
                    function(i) sum(wgtsB[[i]] * muB[IDsB[[i]]]))

# 3b)

# Here, a "selection" matrix is created, where each weight is
# built into the jth index for the ith row, and then the two 
# matricies are multiplied together to create a linear algebra
# solution for the problem.  This method gives about a two 
# order of magnitude speed up for both 3a and 3b.

ptm <- proc.time()
my_sums_A <- sapply(1:length(IDsA), 
                    function(i) sum(wgtsA[[i]] * muA[IDsA[[i]]]))
proc.time() - ptm

selection_matrix_A <- Matrix(0, 
                             nrow=length(IDsA), 
                             ncol=length(muA),sparse=TRUE)

for (i in (1:length(IDsA)))
{
  print(i)
  selection_matrix_A[i,IDsA[[i]]] <- wgtsA[[i]]
}
ptm <- proc.time()
my_sums_v2_A <- selection_matrix_A %*% muA
proc.time() - ptm
unlist(my_sums_v2_A[,1])

# 3c)

ptm <- proc.time()
my_sums_B <- sapply(1:length(IDsB), 
                    function(i) sum(wgtsB[[i]] * muB[IDsB[[i]]]))
proc.time() - ptm

selection_matrix_B <- Matrix(0, 
                             nrow=length(IDsA), ncol=length(muB),sparse=TRUE)

for (i in (1:length(IDsA)))
{
  vtmp <- i
  # selection_matrix_B[i,IDsB[[i]]] <- wgtsB[[i]]
}
ptm <- proc.time()
my_sums_v2_B <- selection_matrix_B %*% muB
proc.time() - ptm
unlist(my_sums_v2_B[,1])

# Question 4

library(pryr)

# A great option is mem_change and object_size from pryr in the following segments
# the command sort(sapply(ls(),function(x){object_size(get(x))})) ain't a bad way
# of sorting it out

# Size of N_pt4 is negligible
N_pt4 <- 1000000
mem_change(x1 <- rnorm(N_pt4))
mem_change(x2 <- rnorm(N_pt4))
mem_change(x3 <- rnorm(N_pt4))
mem_change(b <- c(1,5,9))

mem_change(y <- x1*b[1] + x2*b[2] + x3*b[3] + rnorm(N_pt4))

# sort(sapply(ls(),function(x){object_size(get(x))}))
# debug(lm)

lm(y ~ x1 + x2 + x3)

# 4b)

# The objects that are bigger than 10% of the original are:

# Browse[2]> object_size(get("mf"))
# 32 MB

# Browse[2]> object_size(get("x"))
# 88 MB

# Browse[2]> object_size(get("y"))
# 64 MB

# Browse[2]> object_size(get("z"))
# 168 MB

# The reasons that the vectors can be more than just 8 bytes x
# the number of elements is because R vectors/lists can feature
# several attributes/pieces of metadata, like the "names" of 
# columns or lists for example, that require variable space. In
# our case, the "names" are just the indices (tautological).
# 

# 4c)

# To reduce the memory usage before lm.fit() I would remove
# unnecessary attributes like $names from the data frame.

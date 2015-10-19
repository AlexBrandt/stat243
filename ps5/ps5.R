# 1 a)

# The number of precision is 1.000000000001 is 13from decimal import *
# NOTE: all python code is just embedded in the .Rnw doc

options(digits=22)
# Create the list, init an empty sum variable, then sum
numbers <- c(1, rep(1 * 10^(-16), 10000))
sum(numbers)

options(digits=22)
# Create the list, init an empty sum variable, then loop
# using the previous numbers list/array
my_sum <- 0
for (i in 1:length(numbers))
{
  my_sum <- my_sum + numbers[i]
}
my_sum

options(digits=22)
# Create the list, init an empty sum variable, then loop
revised_numbers <- c(rep(1 * 10^(-16), 10000), 1)
my_revised_sum <- 0.0
for (i in 1:length(revised_numbers))
{
  my_revised_sum <- my_revised_sum + revised_numbers[i]
}
my_revised_sum

# Find out where 'sum' goes...
sum

# 2) testing speeds

options(digits=6)
v <- rep(as.integer(1), 200000000)
system.time(sum(v))

v <- rep(as.double(1), 200000000)
system.time(sum(v))

v <- rep(as.integer(1:5), 20000000)
system.time(v[(v %% 5) == 0])

v <- rep(as.double(1:5),  20000000)
system.time(v[(v %% 5) == 0 ])

options(digits=6)
v <- rep(as.integer(1), 2000000)
system.time(sapply(v, is.integer))

v <- rep(as.double(1), 2000000)
system.time(sapply(v, is.integer))




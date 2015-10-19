# 1 a)

# The number of precision is 1.000000000001 is 12 (the number of digits after the decimal).

# 1 b) 

numbers <- c(1, rep(1 * 10^(-16), 10000))
sum(numbers)

# Creates just one

# 1 d)

my_sum <- numbers[1] + numbers[2]
for (i in 3:length(numbers))
{
    my_sum <- my_sum + numbers[i]
}
my_sum

# Doesn't work...

# 1 e) works!

revised_numbers <- c(rep(1 * 10^(-16), 10000), 1)
my_revised_sum <- revised_numbers[1] + revised_numbers[2]
for (i in 3:length(revised_numbers))
{
  my_revised_sum <- my_revised_sum + numbers[i]
}
my_revised_sum


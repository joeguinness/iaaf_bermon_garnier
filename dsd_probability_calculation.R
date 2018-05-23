
# population rate of DSD
general_pop_rate <- 1/20000

# binomial probabilities for 849 athletes
cbind(0:6, 100*round(dbinom(0:6, 849, general_pop_rate ),4))

# probability of observing 6 or more
# in a sample of 849 athletes
prob6ormore <- 1 - sum( dbinom(0:5, 849, general_pop_rate ) )
prob6ormore

# reciprocal
1/prob6ormore


# binomial probabilities for n=21 and p=.05
cbind(0:10, 100*round(dbinom(0:10, 21, 0.05), 4))

# probability of 5 or more rejections at level 0.05
1 - sum( dbinom(0:4, 21, 0.05 ) )

# probability of 4 or more rejections at level 0.05
1 - sum( dbinom(0:3, 21, 0.05 ) )



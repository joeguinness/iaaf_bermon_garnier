
# check the t-tests
get_pvalue <- function( n1, m1, s1, n2, m2, s2, lowtail ){
    tstat <- (m1-m2)/sqrt( s1^2/n1 + s2^2/n2 )
    dof <- (s1^2/n1 + s2^2/n2)^2/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
    pt( tstat, dof, lower.tail = lowtail )
}

# we have to guess the sample sizes when
# n is not divisible by 3. This assumes that
# the low and high group sizes are chosen
# to be equal.

# 400m
n1 <- 22
m1 <- 52.60
s1 <- 3.30
n2 <- 22
m2 <- 51.16
s2 <- 1.06

get_pvalue( n1, m1, s1, n2, m2, s2, lowtail = FALSE )


# 400mH
n1 <- 22
m1 <- 57.38
s1 <- 3.53
n2 <- 22
m2 <- 55.78
s2 <- 1.60

get_pvalue( n1, m1, s1, n2, m2, s2, lowtail = FALSE )


# 800m
n1 <- 21
m1 <- 122.68
s1 <- 3.71
n2 <- 21
m2 <- 120.50
s2 <- 2.90

get_pvalue( n1, m1, s1, n2, m2, s2, lowtail = FALSE )


# Hammer
n1 <- 18
m1 <- 67.76
s1 <- 2.75
n2 <- 18
m2 <- 70.83
s2 <- 5.16

get_pvalue( n1, m1, s1, n2, m2, s2, lowtail = TRUE )


# Pole Vault
n1 <- 16
m1 <- 4.41
s1 <- 0.18
n2 <- 16
m2 <- 4.54
s2 <- 0.17

get_pvalue( n1, m1, s1, n2, m2, s2, lowtail = TRUE )






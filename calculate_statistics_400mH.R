
# read in the cleaned data
iaaf <- read.csv("women_iaaf_400mH_2011_2013_wiki_clean.csv", as.is = TRUE)
attach(iaaf)

# remove duplicates within each year by bib
# keep the minimum time for each athlete

# 2011 
inds_year <- year == 2011
athletelist <- unique( athlete[ inds_year ] )
min_athlete <- c()
min_times <- c()
for(j in 1:length(athletelist) ){
    
    inds <- which( athlete[ inds_year ] == athletelist[j] )
    mintime <- min( time[ inds_year ][inds], na.rm = TRUE )
    
    min_athlete <- c( min_athlete, athlete[inds[1]] )
    min_times <- c(min_times, mintime )
}
min_times[ min_times == Inf ] <- NA
min2011 <- data.frame( year = 2011, min_athlete, min_times )

# 2013 
inds_year <- year == 2013
athletelist <- unique( athlete[ inds_year ] )
min_athlete <- c()
min_times <- c()
for(j in 1:length(athletelist) ){
    
    inds <- which( athlete[ inds_year ] == athletelist[j] )
    mintime <- min( time[ inds_year ][inds], na.rm = TRUE )
    
    min_athlete <- c( min_athlete, athlete[ inds_year ][inds[1]] )
    min_times <- c(min_times, mintime )
}
min_times[ min_times == Inf ] <- NA
min2013 <- data.frame( year = 2013, min_athlete, min_times )

# combine the results
min_results <- rbind( min2011, min2013 )

# statistics for all athletes
print( sum( !is.na(min_results$min_times) ) )
print( mean( min_results$min_times, na.rm = TRUE ) )
print( sd(  min_results$min_times, na.rm = TRUE  ) )





# permutation test to get more accurate p-value
notna <- !is.na( min_results$min_times )
y <- min_results$min_times[notna]
ny <- length(y)

group_sizes <- c( 22, 23, 22 )
group_ends <- cumsum(group_sizes)

nsim <- 1000000
diff_vec <- rep(NA,nsim)
for(j in 1:nsim){
    ord <- sample(ny)
    group1 <- y[ord[1:group_ends[1]]]
    group3 <- y[ord[(group_ends[2]+1):group_ends[3]]]
    diff_vec[j] <- mean(group1) - mean(group3)
}
observed_diff <- 57.38 - 55.78 
print( mean( diff_vec > observed_diff ) )




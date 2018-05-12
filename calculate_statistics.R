
# read in the cleaned data
iaaf800 <- read.csv("women_iaaf_800m_2011_2013_clean.csv", as.is = TRUE)
attach(iaaf800)
iaaf800

# remove duplicates within each year by bib
# keep the minimum time for each athlete

# 2011 
inds_year <- year == 2011
biblist <- unique( bib[ inds_year ] )
min_athlete <- c()
min_times <- c()
for(j in 1:length(biblist) ){
    
    inds <- which( bib[ inds_year ] == biblist[j] )
    mintime <- min( time[ inds_year ][inds] )
    
    min_athlete <- c( min_athlete, athlete[inds[1]] )
    min_times <- c(min_times, mintime )
}

min2011 <- data.frame( year = 2011, min_athlete, min_times )

# 2013 
inds_year <- year == 2013
biblist <- unique( bib[ inds_year ] )
min_athlete <- c()
min_times <- c()
for(j in 1:length(biblist) ){
    
    inds <- which( bib[ inds_year ] == biblist[j] )
    mintime <- min( time[ inds_year ][inds] )
    
    min_athlete <- c( min_athlete, athlete[ inds_year ][inds[1]] )
    min_times <- c(min_times, mintime )
}
min2013 <- data.frame( year = 2013, min_athlete, min_times )

# combine the results
min_results <- rbind( min2011, min2013 )

# statistics for all athletes
mean( min_results$min_times, na.rm = TRUE )
sd(  min_results$min_times, na.rm = TRUE  )


# statistics taking out DQed athletes
dq <- c(6,7,8,44)
min_results[dq,]
mean( min_results$min_times[-dq], na.rm = TRUE )
sd(  min_results$min_times[-dq], na.rm = TRUE  )

# there are 68 individual athletes
# two did not finish (NA values in min_results)
# that leaves two to remove in order to get
# n = 64

# loop over every pair of athletes to find
# mean and sd for remaining 64 athletes
n_min <- nrow(min_results)
mean_remove <- matrix(NA, n_min, n_min )
sd_remove <- matrix(NA, n_min, n_min )
for(i in 1:(n_min-1)){
    for(j in (i+1):n_min){
        ind_remove <- c(i,j)
        mean_remove[i,j] <- mean( min_results$min_times[-ind_remove], na.rm = TRUE )
        sd_remove[i,j] <- sd( min_results$min_times[-ind_remove], na.rm = TRUE )
    }
}

# figure out which ones match values in the table
which_match <- which( abs( mean_remove - 121.80 ) < 0.005
       & abs( sd_remove - 5.42 ) < 0.005, arr.ind = TRUE )

# print out years and names
for(j in 1:nrow(which_match)){
    cat( paste( min_results[ which_match[j,1], 1 ], 
                min_results[ which_match[j,1], 2 ]," ",
                min_results[ which_match[j,2], 1 ],
                min_results[ which_match[j,2], 2 ], "\n" ) )
}










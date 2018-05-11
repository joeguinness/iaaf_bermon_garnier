
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

min2011 <- data.frame( min_athlete, min_times )

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
min2013 <- data.frame( min_athlete, min_times )

# combine the results
min_results <- rbind( min2011, min2013 )

# statistics for all athletes
mean( min_results$min_times, na.rm = TRUE )
sd(  min_results$min_times, na.rm = TRUE  )


# statistics taking out DQed athletes
dq <- c(6,7,8,44)
min_results[dq,]
mean( min_results$min_times[-savinova], na.rm = TRUE )
sd(  min_results$min_times[-savinova], na.rm = TRUE  )









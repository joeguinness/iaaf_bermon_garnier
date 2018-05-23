

# function for converting hh:mm:ss.xx to seconds
convert_to_seconds <- function( time_string ){
    
    tsplit <- as.numeric( strsplit(time_string,":")[[1]] )
    nsplit <- length(tsplit)
    if( nsplit == 1 ){         #  ss.xx
        seconds <- tsplit[1]
    } else if( nsplit == 2 ){     #  mm:ss.xx
        seconds <- 60*tsplit[1] + tsplit[2]
    } else if( nsplit == 3 ){  #  hh:mm:ss.xx
        seconds <- 60^2*tsplit[1] + 60*tsplit[2] + tsplit[3]
    }
    return(seconds)
}


# function for cleaning the data
clean_data <- function( event ){

    # read in the raw data
    fname <- paste0("women_iaaf_",event,"_2011_2013_wiki_raw.csv")
    rawdata <- read.csv(fname,header= TRUE, as.is = TRUE)
    n <- nrow(rawdata)

    # convert times to seconds
    rawtime <- rawdata$time
    time_seconds <- rep(NA,n)
    for(j in 1:n){
        time_seconds[j] <- convert_to_seconds( rawtime[j] )
    }

    # create cleaned data frame
    cleaned_data <- data.frame(
        year = rawdata$year,
        position = rawdata$place,
        round = rawdata$round,
        heat = rawdata$heat,
        athlete = rawdata$athlete,
        country = rawdata$country,
        time = time_seconds,
        notes = rawdata$notes)

    # save the data
    fname <- paste0("women_iaaf_",event,"_2011_2013_wiki_clean.csv")
    write.csv( cleaned_data, file = fname, row.names = FALSE )
}

clean_data( "400m" )
clean_data( "400mH" )
clean_data( "800m" )
clean_data( "1500m" )


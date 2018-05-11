

# read in the raw data
rawdata <- read.csv("women_iaaf_800m_2011_2013_raw.csv", 
    header= TRUE, as.is = TRUE)
n <- nrow(rawdata)

# convert times to seconds
rawtime <- rawdata$time
time_seconds <- rep(NA,n)
for(j in 1:n){
    stringtime <- rawtime[j]
    split_colon <- strsplit(stringtime,":")
    minutes <- split_colon[[1]][1]
    split_rest <- strsplit(split_colon[[1]][2]," ")
    seconds <- split_rest[[1]][1]
    time_seconds[j] <- as.numeric(minutes)*60 + as.numeric(seconds)
}

# create cleaned data frame
cleaned_data <- data.frame(
    year = rawdata$year,
    position = rawdata$position,
    round = rawdata$round,
    heat = rawdata$heat,
    bib = rawdata$bib,
    athlete = rawdata$name,
    country = substr(rawdata$country,1,3),
    time = time_seconds,
    notes = rawdata$notes)

# save the data
write.csv( cleaned_data, file = "women_iaaf_800m_2011_2013_clean.csv", row.names = FALSE )


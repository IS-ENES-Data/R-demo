#extracts subperiod from daily data
#input: period: numric vector with two elements c(startear, number of years)
# e.g. period = c(1980,15)

get_period.day <- function(bd, period) {
  pattern <- as.character(period[1])
  for ( i in period[1]:(period[1]+period[2]-2)) {
    pattern<- paste(pattern,i+1, sep="|")
  }
  pattern  
  bsub <-bd[[grep(pattern, names(bd))]]
  return(bsub)
}
### function
## computes annual regional average from daily as well as monthly data

get_annAvg <- function(bd,dir,var='var', unit ='[unit of variable]') {

  ##compute regional mean for each day
  avg.bd <- cellStats(bd, "mean")
  sd.bd <-cellStats(bd, "sd")

  if( all(is.na(avg.bd))) {
    stop("all cells in this region are masked/ NaN. Check modeling realm of data.")
  } 
  ##dataframe of time series of average in regions and their standard deviations
  stat.bdDf <- data.frame(avg.bd=avg.bd, sd.bd=sd.bd)
  #head(stat.bdDf)

  dates <- gsub("X", row.names(stat.bdDf),replacement="")
  stat.bdDf$dates <- as.Date(dates,"%Y.%m.%d")
  #class(stat.bdDf$dates)
  #get years
  stat.bdDf$year  <- format(stat.bdDf$dates,format="%Y")


   years <- unique(stat.bdDf$year)

  #(stat.bdDf$year)
  avg <-matrix(0,length(years),2)
  colnames(avg)<- c("annMean", "annSd")
  rownames(avg)<- years
  for (i in 1:length(years)) {
    sub <- stat.bdDf[stat.bdDf$year== years[i],]
    avg[i,1]<- mean(sub$avg.bd)
    avg[i,2]<- mean(sub$sd.bd)
  }


  png(filename=paste0(dir,"/annAvg.png"))
  par(mfrow=c(2,1))
  par(mar=c(0,5,3,1))
  years <- as.numeric(years)

  trend <- lm(avg[,1] ~ years)

  slope <- trend$coefficients[2]


  plot(years, avg[,1],main="annual averages of regional mean and standard deviation of daily data", ylab=paste('mean in ', unit),type = "l", col = "red")
  grid()
  abline(trend,col="green")
  mtext( paste("linear Trend [unit]/timePeriod=",length(years)* slope), line =-2, col="green")
  par(mar=c(3,5,0,1))
  if (any(is.na(sd.bd)==FALSE)){
  plot(years, avg[,2],ylab=paste("Standard deviation in ", unit) ,type = "l", col="blue")
  grid() }
  dev.off()
}

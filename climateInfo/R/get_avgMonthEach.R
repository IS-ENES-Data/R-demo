## calculates time series of the spatial average and the corresponding spatial standardderivation
##  in area for each month seperatley
## creates dataframe with 12 colums, for each month and a row or each year
## entries are the calculated mean / sd rom corresponding rasterLayers
get_avgMonthEach <- function( bSub,dir ,var="var", unit="[unit of variable]"){

  bSubJan <- bSub[[which (1:nlayers(bSub) %% 12 ==1)]] #monthly observations always begin with jan?
  janAvg <- cellStats(bSubJan, "mean")
  if( all(is.na(janAvg))) {
    stop("all cells in this region are masked/ NaN. Check modeling realm of data.")
  }
  monthAvg <- data.frame("jan"= janAvg ,"feb"=0,'mar'=0, 'apr'=0, 'may'=0, 'jun'=0, 'jul'=0, 'aug'=0, 'sep'=0, 'oct'=0, 'nov'=0, 'dec'=0)


  for ( i in 2:12) {
    # v<- c( 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    if(i==12) {bSubMonths <- bSub[[ which (1:nlayers(bSub)%%12 == 0)]]}
    else {bSubMonths <- bSub[[which (1:nlayers(bSub)%%12 == i)]]}
    #print(bSubMonths)

    monthAvg[,i] <-  cellStats(bSubMonths, "mean")

  }
  #monthAvg
  t <- 1:nrow( monthAvg)
  dates <- gsub("X", row.names(monthAvg),replacement="")
  dates <- as.Date(dates,"%Y.%m.%d")
  years <- unique(format(dates,"%Y"))
  as.numeric(years)
  png(filename=paste0(dir,"/annAvgEachMon.png"))
  par(mfrow = c(4, 3))  # 4 rows and 3 columns
  par(mgp=c(2,1,0))
  for (i in 1: ncol(monthAvg)) {
    trend <- lm(monthAvg[,i] ~ seq(years[1], years[length(years)]))
    #print(trend)
    slope <- trend$coefficients[2]
    # print(slope)
    plot(years, monthAvg[,i], type = "l", col = "red", xlab="Year",
         ylab=paste('mean of ', var, 'in ', unit),
         main = colnames(monthAvg[i]) )
    grid()

    abline( trend  ,col="green")
    mtext(slope* length(years) ,cex=0.9 )
  }
  dev.off()
}

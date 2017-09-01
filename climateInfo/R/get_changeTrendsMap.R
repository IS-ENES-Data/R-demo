## get_changeTrendsMap
## Spatial characteristics of the variable change trends
## a linear model of values in relation with time will be calculated for each cell
## over time period, i.e the corresponding layers
## input: rasterObject with data

get_changeTrendsMap <- function(b, mapproj=NULL,dir, unit = "[unit of variable]") {
  time <- 1:nlayers(b)
  fun=function(x) { if (is.na(x[1]) ||is.na(x[length(x)]) ){ NA }
    else { lm(x ~time )$coefficients[2] }}
  slope <- calc(b, fun)
  slope <- slope*length(time)
  png(filename=paste0(dir,"/mapTrend.png"))
  trendMap<-plot(slope,col = colorRampPalette(c("black",'blue', "darkgreen",'green',
                                                "yellow",'pink','red','maroon'),interpolate='spline')(50 ),
                 main= sprintf("Change trends %s/ timePeriod", unit) )
  grid()
  if( !(is.null(mapproj))){
    maps::map(mapproj, add=TRUE) }
  else {maps::map(add=TRUE)}
  dev.off()
}

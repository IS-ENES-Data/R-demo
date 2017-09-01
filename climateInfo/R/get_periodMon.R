#extract time period for monthly climate data
#input:
#b: rasterBrickobject containing climate data
#vector with two entries: (startyear, period): startyear from which on period number years
#                                                   should be extracted.
#example (years =c(2008, 20)) for monthly data

get_periodMon <- function(b, years ){
  start <- as.character ( years[1])
  x <- grep(  start,names(b))
  if (length(x)==0){
    stop("data doesn't contain information for this year")
  }
  if ((x[1]+12*years[2]-1)> nlayers(b)){
    warning("selectd period exceeds observation period of data!")
    bsub <- b[[x[1]:nlayers(b)]]
  }
  else { bsub <- b[[x[1]:(x[1]+12*years[2]-1)]]}
  return (bsub)
}

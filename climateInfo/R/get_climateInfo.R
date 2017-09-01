#Input parameter
#region: vector of four numbers ( xmin, max, ymin, ymax) which defines the spatial exract for which
#        the inormation is required
#time:  ##tartdate, enddate?? #startdate, period?? #startyear? -month?
## vector with two entries  c(startyear, period). period: number of years



# annual  (trend)?  monthly ? one month over period?


get_climateInfo <- function (file, var,region=NULL, time=NULL){

  ##get attibutes from netcdf4 data file
  nc<- ncdf4::nc_open(file)
  frequency <- ncdf4::ncatt_get(nc,0,"frequency")$value
  unit<- ncdf4::ncatt_get(nc,var,"units")$value
  ncdf4::nc_close(nc)

    ##currently only daily and monthly data will be taken.
   if (!(frequency=="mon")& !(frequency=="day"))
  {stop("invalid frequency of observation! please choose data with time frequency
         'mon' or day'")}


  b <- brick(file, varname=var) #read data as raster object


   if (isLonLat(b)) { #function only works for objects with lon/lat crs

    if (xmin(extent(b))> -5) {  # check spectre of longitude, how projection is centered
      #center is amrika, 0-360
      mapproj <-"world2"
    } else {mapproj = NULL } #center in greenwich

    if (!is.null(region)){
      ext <- extent (region)
      if (xmin(ext) < xmin(extent(b)) ||xmax(ext) > xmax(extent(b)) ) { #check if required are is contained in data
        stop("This area is not contained in data. Check also centering of projction. (-180-180 or 0-360?)")
      }
      bSub <- crop (b,ext)
    }

       else {bSub <- b} }
  else {stop(" not a good coordinate reference system( Long/Lat)")}



  ##create folder where results (plots and data in the end) get saved
  dir <- create_folder(file, var=var, t=time, reg=region )

  if (frequency == "mon"){


    if (!is.null(time)){       # extract layers corresponding to required time period
        bSub <-get_periodMon( bSub , time)
      }
    ##annual averages of spatial means and standardderivation
    get_annAvg(bSub, dir,var=var, unit=unit)


    ## linear change trends
    get_changeTrendsMap(bSub, mapproj,dir,  unit=unit)

    ## time series of monthly averages for each month seperatley over period
    get_avgMonthEach(bSub,dir, var =var, unit=unit)

     } else if (frequency =="day") {

       if (!is.null(time)){       # extract layers corresponding to required time period
         bSub <-get_period.day( bSub , time)
       }

    ##for daily data observations, annual means will be computed
    get_annAvg(bSub,dir, var, unit)
    get_changeTrendsMap(bSub, mapproj,dir=dir)
    ##monthly means
   # get_monAvg_day(bSub)
  }


  #if requested, write data as csv
  yn <- readline("Do you want to have data as csv file, saved in directory defined above? [y/n]")
  while(!(yn %in% c("y", "n"))) {
    yn<- readline("Please enter 'y' or 'n'!: ")}
  if (yn =="y") {
    print("Data are saved in given directory in csv format")
    saveData(bSub, dir)
  }

}

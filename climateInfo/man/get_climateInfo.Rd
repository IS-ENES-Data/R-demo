\name{get_climateInfo}
\alias{get_climateInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_climateInfo}
\description{ This function calls, depending on input data, a selection of functions that are performed on  data to receive some information about it.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_climateInfo(file, var, region=NULL, time=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{netCDF4 file with climate data; (as in esgf-dkrz)}

  \item{var}{ characterstring of the file's variable shortname, e.g. "tas", must(!) be extactly the name as defined in netcdf4 data file.}

  \item{region}{ numeric vector with four elements describing rectangular area   that should be extracted by coordninates; c(lonmin,lonmax, latmin,latmax); if   NULL(by default) no subst will be created}

  \item{time}{numeric vector wit two entries describing startyear and length    of required time period in years, e.g c(2020,20). Data ro this period will be   extracted; if NULL (by default) the whole timeserie wil be submitted}

}
\details{Main function of package which is used by the user. All functions that perform operations on data are called in get_climateInfo(). First there are some checks if data fits frequency(mon or day) and coordinate reference system (lon/lat) requirements.  If ok, required subset will be extracted, from which plots will be calculated.  }

\value{ no actual value. Infomation is given in form of plots, saved in folder in given directory
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Carolin Natemeyer
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
R package "raster"
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }% use one of  RShowDoc("KEYWORDS")
\keyword{ climate data }% __ONLY ONE__ keyword per line
\keyword {netcdf4}

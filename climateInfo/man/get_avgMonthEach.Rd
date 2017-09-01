\name{get_avgMonthEach}
\alias{get_avgMonthEach}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_avgMonthEach.R
%%  ~~function to do ... ~~
}
\description{
calculates time series of the spatial average and the corresponding spatial standarddeviation in area for each month seperatley
}
\usage{
get_avgMonthEach(bSub,dir, var="var", unit="[unit of variable]")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bSub}{ object of class rasterBrick
}
  \item{dir} {characterstring, describing directory where image should get       saved}
}
\details{
The function creates dataframe with 12 colums, for each month and a row or each year; entries are the calculated mean / sd from corresponding rasterLayers. Rows correspond to time series.
na values will be ignored, but if all cells in region are masked, then error.
}
\value{
Plot of timeseries of regional average, linear trend model and standardeviation or each month.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{C.Natemeyer
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ raster }% use one of  RShowDoc("KEYWORDS")
\keyword{ spatial }% __ONLY ONE__ keyword per line

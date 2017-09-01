\name{get_changeTrendsMap}
\alias{get_changeTrendsMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_changeTrendsMap
%%  ~~function to do ... ~~
}
\description{ This function calculates a linear model (values ~ timeperiod) for each cell over the entire time period }
\usage{
get_changeTrendsMap(b, mapproj= NULL, dir )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ object from class rasterBrick, each layer contains geographic data   of the corresponding time point }
  \item{mapproj}{describing centering of geographic data, e.g. lonitudes from
  0-360 degrees (world2), value will get passed to function 'map' to draw a      backround map in plot. If NULL then greenwich is centered by default.}
  \item{dir}{characterstring, describing irectory where image should get saved}
}
}
\details{
linear model for each cell; data  ~ sequence of observations(either daily or monthly, dependin on data)
}
\value{ a spatial plot with a map from reqired region and the change trend per period for each cell is saved }
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
\keyword{linear model}

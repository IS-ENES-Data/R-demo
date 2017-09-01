\name{get_period.day}
\alias{get_period.day}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_period.day
%%  ~~function to do ... ~~
}
\description{
The function extracts required period from daily data.
}
\usage{
get_period.day(bd, period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bd}{rasterBrick object, must contain daily data, i.e one layer for one day
%%     ~~Describe \code{x} here~~
}
\item{period}{numeric vector with two elements, first element is the start year rom which on one needs data, second the length of the period in years. Eample period= c(1980,15) }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a subset of the inputbrick which contains data for the required time.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
C.N.
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
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

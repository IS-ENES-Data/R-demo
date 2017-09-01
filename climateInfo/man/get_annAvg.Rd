\name{get_annAvg}
\alias{get_annAvg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_annAvg
%%  ~~function to do ... ~~
}
\description{
computes annual averages of regional mean and standarddeviation from data
}
\usage{
get_annAvg_day(b, dir, var="var", unit="unit")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ rasterBRick objeckt containing  data o daily or monthly obervations
%%     ~~Describe \code{x} here~~
}
\item{dir}{characterstring; path to direction where images should be saved}
\item{var}{ characterstring, name of variable}
\item{unit}{characterstring, unit of variable}
}
\details{ na values / masked cells will be ignored when computing regional means. If all cells  in region are masked (e.g. region is over ocean while data modeling realm is land) there will be an error.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ plot of time serie from annual means and standarddeviations
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Carolin Natemeyer
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

\name{saveData}
\alias{saveData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{saveData()
%%  ~~function to do ... ~~
}
\description{ creates csv file containin the required data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
saveData(bSub, dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bSub}{brick with data subset that should be written as csv file.
%%     ~~Describe \code{x} here~~
}
\item{dir}{ path to directory where file hould be created.}
}
\details{';'-separated, first two columns describe x and y coordinate of cell where data was observed.
saved in same folder where images are saved.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{create_folder}
\alias{create_folder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{create_folder
%%  ~~function to do ... ~~
}
\description{
This function creates a directory where images and data will be saved
}
\usage{
create_folder(file, var, t, reg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ filename od datafile which must be of format netcdf4
%%     ~~Describe \code{x} here~~
}
  \item{var}{characterstring describing observed variable, e.g "tas"}

  \item{t}{numeric vector of extracted time period with  two elements, c(startyear, period [in years])}

  \item{reg} {numeric vector with four elements describin rectagular area which is extracted from data, c(xmin,xmax,ymin,ymax)}
}
\details{ User is asked to enter a directory to where the folder should be created. Naming convention of folder: 'var.project_id.model_id.timePeriod.area'. information of project_i, model_id is taken directly from lobal attributes of netcdf4 file.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns characterstring which describes directory of folder
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

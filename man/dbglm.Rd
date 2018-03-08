\name{dbglm}
\alias{dbglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast generalised linear models in a database
}
\description{
Fit a generalised linear model to a large dataset, by fitting the model to a subsample and using the subsample estimate as the starting value for one iteration of Fisher scoring. The one-step update is computed in a \code{dbplyr} expression that will translate to a single database query.
}
\usage{
dbglm(formula, family = binomial(), tbl, sd = FALSE, weights = .NotYetImplemented(), subset = .NotYetImplemented(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A model formula. It can have interactions but cannot have any transformations except \code{factor}
}
  \item{family}{
Model family
}
  \item{tbl}{
An object inheriting from \code{tbl}. Will typically be a database-backed lazy \code{tbl} from the \code{dbplyr} package
}
  \item{sd}{
Experimental: compute the standard deviation of the score as well as the mean in the update and use it to improve the information matrix estimate
}
  \item{weights}{
We don't support weights
}
  \item{subset}{
If you want to analyze a subset, use \code{filter()} on the data
}
  \item{\dots}{
yada yada yada
}
}
\details{
For a dataset of size \code{N} the subsample is of size \code{N^(5/9)}. Unless \code{N} is large the approximation won't be very good. Also, with small \code{N} it's quite likely that, eg, some factor levels will be missing in the subsample. 
}
\value{
A list with elements
  \item{tildebeta }{coefficients from subsample}
  \item{hatbeta }{final estimate}
  \item{tildeV }{variance matrix from subsample}
  \item{hatV }{final estimate}

}
\references{
\url{http://notstatschat.tumblr.com/post/171570186286/faster-generalised-linear-models-in-largeish-data}
}


\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% use one of  RShowDoc("KEYWORDS")


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.ContinuousDeletor}
\alias{summary.ContinuousDeletor}
\alias{ContinuousDeletor.summary}
\alias{summary,ContinuousDeletor-method}

\title{Summarize the properties of an object}

\description{
      Summarize the properties of an object.
}

\usage{
\method{summary}{ContinuousDeletor}(object, ...)
}

\arguments{
      \item{object}{An object}
      \item{...}{Not used.}
}

\value{
 Returns a PSRootSummary object.
}

\examples{

      # create an object
      a<-ContinuousDeletor(rate=1,dist=expression(rnorm(1,mean=5,sd=3)), max.length=10)
      # get a summary
      summary(a)
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{ContinuousDeletor}}.
}

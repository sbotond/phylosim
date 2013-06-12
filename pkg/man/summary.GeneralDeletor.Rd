%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.GeneralDeletor}
\alias{summary.GeneralDeletor}
\alias{GeneralDeletor.summary}
\alias{summary,GeneralDeletor-method}

\title{Summarize the properties of an object}

\description{
      Summarize the properties of an object.
}

\usage{
\method{summary}{GeneralDeletor}(object, ...)
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
      a<-GeneralDeletor(rate=1,name="Del Bosque")
      # get a summary
      summary(a)
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{GeneralDeletor}}.
}

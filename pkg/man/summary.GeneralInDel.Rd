%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.GeneralInDel}
\alias{summary.GeneralInDel}
\alias{GeneralInDel.summary}
\alias{summary,GeneralInDel-method}

\title{Summarize the properties of an object}

\description{
      Summarize the properties of an object.
}

\usage{
\method{summary}{GeneralInDel}(object, ...)
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
      a<-GeneralInDel(rate=1,propose.by=function(process){sample(c(1,2,3),1)})
      # get a summary
      summary(a)
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{GeneralInDel}}.
}

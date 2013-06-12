%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.BinarySubst}
\alias{summary.BinarySubst}
\alias{BinarySubst.summary}
\alias{summary,BinarySubst-method}

\title{Summarize the properties of an object}

\description{
      Summarize the properties of an object.
}

\usage{
\method{summary}{BinarySubst}(object, ...)
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
      p<-BinarySubst(rate=0.25,name="Binary",rate.list=list("0->1"=2,"1->0"=1))
      # get a summary
      summary(p)
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{BinarySubst}}.
}

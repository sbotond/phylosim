%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.ToleranceSubstitution}
\alias{summary.ToleranceSubstitution}
\alias{ToleranceSubstitution.summary}
\alias{summary,ToleranceSubstitution-method}

\title{Summarize the properties of an object}

\description{
      Summarize the properties of an object.
}

\usage{
\method{summary}{ToleranceSubstitution}(object, ...)
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
      a<-ToleranceSubstitution(alphabet=BinaryAlphabet(),rate.list=list("0->1"=1,"1->0"=2))
      # get a summary
      summary(a)
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{ToleranceSubstitution}}.
}

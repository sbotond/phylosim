%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{clone.Process}
\alias{clone.Process}
\alias{Process.clone}
\alias{clone,Process-method}

\title{Clone a process object}

\description{
      Clone a process object.
      Write protection is set to FALSE for the new Process object.
}

\usage{
\method{clone}{Process}(this, ...)
}

\arguments{
      \item{this}{A Process object.}
      \item{...}{Not used.}
}

\value{
      A Process object.
}

\examples{
      # create a process object
      p<-Process()
      # clone process object
      pp<-clone(p)
      # check identity
      p == pp
}

\author{Botond Sipos, Gregory Jordan}

\seealso{
      For more information see \code{\link{Process}}.
}

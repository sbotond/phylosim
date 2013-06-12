%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.DiscreteInsertor}
\alias{plot.DiscreteInsertor}
\alias{DiscreteInsertor.plot}
\alias{plot,DiscreteInsertor-method}

 \title{Plot the insertion length distribution}

 \description{
       Plot the insertion length distribution.
 }

 \usage{
\method{plot}{DiscreteInsertor}(x, ...)
}

 \arguments{
       \item{x}{A DiscreteInsertor object.}
       \item{...}{Not used.}
 }

 \value{
       The DiscreteInsertor object (invisible).
 }

 \examples{
       i<-DiscreteInsertor(
               name="MyDiscIns",
               sizes=1:6,
               probs=c(0.25000000, 0.16666667, 0.08333333, 0.08333333, 0.16666667, 0.25000000)
       )
       # plot the insertion length distribution
       plot(i)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{DiscreteInsertor}}.
 }

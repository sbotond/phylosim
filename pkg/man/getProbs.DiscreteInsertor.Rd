%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProbs.DiscreteInsertor}
\alias{getProbs.DiscreteInsertor}
\alias{DiscreteInsertor.getProbs}
\alias{getProbs,DiscreteInsertor-method}

 \title{Get the insertion length probabilities}

 \description{
       Get the insertion length probabilities.
 }

 \usage{
\method{getProbs}{DiscreteInsertor}(this, ...)
}

 \arguments{
       \item{this}{A DiscreteInsertor object.}
       \item{...}{Not used.}
 }

 \value{
       A numeric vector with the insertion length probabilities.
 }

 \examples{
       # create a DiscreteInsertor object
       i<-DiscreteInsertor(rate=1, sizes=1:3)
       # set/get length probabilities
       setProbs(i,c(1/3,1/3,1/3)) # equal probabilites
       getProbs(i)
       # set/get length probabilities via virtual field
       x<-c(2,2,1)
       # normalize x
       x<-x/sum(x)
       i$probs<-x
       i$probs
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{DiscreteInsertor}}.
 }

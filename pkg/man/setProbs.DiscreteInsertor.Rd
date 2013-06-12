%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProbs.DiscreteInsertor}
\alias{setProbs.DiscreteInsertor}
\alias{DiscreteInsertor.setProbs}
\alias{setProbs,DiscreteInsertor-method}

 \title{Set the insertion length probabilities}

 \description{
       Set the insertion length probabilities.

       The \code{sizes} virtual field must be set before setting the length probabilities.
       The length of the provided numeric vector must match with the length of the vector
       stored in the \code{sizes} virtual field. The vector is rescaled if the values do not
       sum to one and a warning is issued.
 }

 \usage{
\method{setProbs}{DiscreteInsertor}(this, value, ...)
}

 \arguments{
       \item{this}{A DiscreteInsertor object.}
       \item{value}{A numeric vector containg the length probabilities.}
       \item{...}{Not used.}
 }

 \value{
       The vector of probabilities.
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

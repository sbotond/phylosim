%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setSizes.DiscreteInsertor}
\alias{setSizes.DiscreteInsertor}
\alias{DiscreteInsertor.setSizes}
\alias{setSizes,DiscreteInsertor-method}

 \title{Set the sizes of the proposed insertions}

 \description{
       Set the sizes of the proposed insertions.
     
       The provided numeric vector is rounded.
 }

 \usage{
\method{setSizes}{DiscreteInsertor}(this, value, ...)
}

 \arguments{
       \item{this}{A DiscreteInsertor object.}
       \item{value}{A numeric vector.}
       \item{...}{Not used.}
 }

 \value{
       A vector of integers (invisible).
 }

 \examples{
       # create a DiscreteInsertor object
       i<-DiscreteInsertor(rate=1)
       # set insertion sizes    
       setSizes(i,c(1,2,3))
       # get insertion sizes
       getSizes(i)
       # set/get sizes via virtual field
       i$sizes<-1:10
       i$sizes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{DiscreteInsertor}}.
 }

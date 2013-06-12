%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSizes.DiscreteInsertor}
\alias{getSizes.DiscreteInsertor}
\alias{DiscreteInsertor.getSizes}
\alias{getSizes,DiscreteInsertor-method}

 \title{Get the sizes of the proposed insertions}

 \description{
       Get the sizes of the proposed insertions.
 }

 \usage{
\method{getSizes}{DiscreteInsertor}(this, ...)
}

 \arguments{
       \item{this}{A DiscreteInsertor object.}
       \item{...}{Not used.}
 }

 \value{
       A vector of integers.
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

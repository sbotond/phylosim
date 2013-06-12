%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setBaseFreqs.F84}
\alias{setBaseFreqs.F84}
\alias{F84.setBaseFreqs}
\alias{setBaseFreqs,F84-method}

 \title{Set the base frequency parameters}

 \description{
       Set the base frequency parameters.

       The order of the frequency parameters must match with the order of symbols
       in the NucleotideAlphabet objects.
 }

 \usage{
\method{setBaseFreqs}{F84}(this, value, ...)
}

 \arguments{
       \item{this}{A F84 object.}
       \item{value}{A vector of base frequencies.}
       \item{...}{Not used.}
 }

 \value{
       value (invisible)
 }

 \examples{
       # construct object
       p<-F84()
       # set/get base frequency parameters
       setBaseFreqs(p,c(2,1,2,1)/6)
       getBaseFreqs(p)
       # set/get base frequency parameters via virtual field
       p$baseFreqs<-c(4,4,1,1)/10
       p$baseFreqs
       # get object summary
       summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{F84}}.
 }

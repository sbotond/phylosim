%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBaseFreqs.HKY}
\alias{getBaseFreqs.HKY}
\alias{HKY.getBaseFreqs}
\alias{getBaseFreqs,HKY-method}

 \title{Get the base frequency parameters}

 \description{
	Get the base frequency parameters.

	The order of the frequency parameters must match with the order of symbols
	in the NucleotideAlphabet objects.
 }

 \usage{
\method{getBaseFreqs}{HKY}(this, ...)
}

 \arguments{
 	\item{this}{An HKY object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the base frequency parameters.
 }

 \examples{
	# construct object
	p<-HKY()
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
 	For more information see \code{\link{HKY}}.
 }

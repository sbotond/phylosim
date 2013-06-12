%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setBaseFreqs.TN93}
\alias{setBaseFreqs.TN93}
\alias{TN93.setBaseFreqs}
\alias{setBaseFreqs,TN93-method}

 \title{Set the base frequency parameters}

 \description{
	Set the base frequency parameters.

	The order of the frequency parameters must match with the order of symbols
	in the NucleotideAlphabet objects.
 }

 \usage{
\method{setBaseFreqs}{TN93}(this, value, ...)
}

 \arguments{
 	\item{this}{A TN93 object.}
	\item{value}{A vector of base frequencies.}
 	\item{...}{Not used.}
 }

 \value{
 	value (invisible)
 }

 \examples{
	# construct a TN93 object
	p<-TN93()
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
 	For more information see \code{\link{TN93}}.
 }

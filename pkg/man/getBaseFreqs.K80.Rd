%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBaseFreqs.K80}
\alias{getBaseFreqs.K80}
\alias{K80.getBaseFreqs}
\alias{getBaseFreqs,K80-method}

 \title{Get the base frequency parameters}

 \description{
	Get the base frequency parameters.

	The order of the frequency parameters must match with the order of symbols
	in the NucleotideAlphabet objects. The K80 model has equal base frequencies.
 }

 \usage{
\method{getBaseFreqs}{K80}(this, ...)
}

 \arguments{
 	\item{this}{A K80 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the base frequency parameters.
 }

 \examples{
	# construct object
	p<-K80()
	# get base frequency parameters
	getBaseFreqs(p) # uniform
	# set/get base frequency parameters via virtual field
	p$baseFreqs # uniform
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K80}}.
 }

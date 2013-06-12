%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBaseFreqs.K81}
\alias{getBaseFreqs.K81}
\alias{K81.getBaseFreqs}
\alias{getBaseFreqs,K81-method}

 \title{Get the base frequency parameters}

 \description{
	Get the base frequency parameters.

	The order of the frequency parameters must match with the order of symbols
	in the NucleotideAlphabet objects. The K81 model has equal base frequencies.
 }

 \usage{
\method{getBaseFreqs}{K81}(this, ...)
}

 \arguments{
 	\item{this}{A K81 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the base frequency parameters.
 }

 \examples{
	# construct object
	p<-K81()
	# get base frequency parameters
	getBaseFreqs(p) # uniform
	# set/get base frequency parameters via virtual field
	p$baseFreqs # uniform
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K81}}.
 }

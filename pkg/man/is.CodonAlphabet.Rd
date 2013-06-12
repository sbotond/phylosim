%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.CodonAlphabet}
\alias{is.CodonAlphabet.default}
\alias{is.CodonAlphabet}

 \title{Check if an object inherits from CodonAlphabet}

 \description{
		Check if an object inherits from CodonAlphabet.
 }

 \usage{
\method{is.CodonAlphabet}{default}(this, ...)
}

 \arguments{
		\item{this}{An object.}
		\item{...}{Not used.}

 }

 \value{
	TRUE of FALSE.
 }

 \examples{
	# create some objects
	a<-CodonAlphabet()
	p<-Process()
	# check if they inherit from CodonAlphabet
	is.CodonAlphabet(a)
	is.CodonAlphabet(p)
 }

 \author{Botond Sipos, Gregory Jordan}


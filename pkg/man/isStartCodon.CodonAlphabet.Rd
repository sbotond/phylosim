%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{isStartCodon.CodonAlphabet}
\alias{isStartCodon.CodonAlphabet}
\alias{CodonAlphabet.isStartCodon}
\alias{isStartCodon,CodonAlphabet-method}

 \title{Check if a codon is a start codon}

 \description{
	Check if a codon is a start codon.
 }

 \usage{
\method{isStartCodon}{CodonAlphabet}(this, codon, ...)
}

 \arguments{
 	\item{this}{A CodonAlphabet object.}
 	\item{codon}{The codon to be checked.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a CodonAlphabet object
	a<-CodonAlphabet()
	# check some codons
	isStartCodon(a,"ATG")
	isStartCodon(a,"TGA")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }

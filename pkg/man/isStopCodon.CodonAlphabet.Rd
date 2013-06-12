%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{isStopCodon.CodonAlphabet}
\alias{isStopCodon.CodonAlphabet}
\alias{CodonAlphabet.isStopCodon}
\alias{isStopCodon,CodonAlphabet-method}

 \title{Check if a codon is a stop codon}

 \description{
	Check if a codon is a stop codon.
 }

 \usage{
\method{isStopCodon}{CodonAlphabet}(this, codon, ...)
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
	isStopCodon(a,"ATG")
	isStopCodon(a,"TGA")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }

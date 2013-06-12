%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{translateCodon.CodonAlphabet}
\alias{translateCodon.CodonAlphabet}
\alias{CodonAlphabet.translateCodon}
\alias{translateCodon,CodonAlphabet-method}

 \title{Translate a codon}

 \description{
	Translate a codon.
 }

 \usage{
\method{translateCodon}{CodonAlphabet}(this, codon, ...)
}

 \arguments{
 	\item{this}{A CodonAlphabet object.}
 	\item{codon}{The codon to be translated.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector containing an amino acid IUPAC code.
 }

 \examples{
	# create a CodonAlphabet object
	a<-CodonAlphabet()
	# translate a codon
	translateCodon(a,"AGG")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }

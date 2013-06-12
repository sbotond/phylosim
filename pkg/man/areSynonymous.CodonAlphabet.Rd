%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{areSynonymous.CodonAlphabet}
\alias{areSynonymous.CodonAlphabet}
\alias{CodonAlphabet.areSynonymous}
\alias{areSynonymous,CodonAlphabet-method}

 \title{Check whether two codons are synonymous}

 \description{
	Check whether two codons are synonymous.
 }

 \usage{
\method{areSynonymous}{CodonAlphabet}(this, codons, ...)
}

 \arguments{
 	\item{this}{A CodonAlphabet object.}
	\item{codons}{A vector containing two codons.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a CodonAlphabet object
	a<-CodonAlphabet()
	areSynonymous(a,c("TCC","TCT"))
	areSynonymous(a,c("TCC","CCT"))
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }

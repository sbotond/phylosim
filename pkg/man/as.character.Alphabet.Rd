%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.Alphabet}
\alias{as.character.Alphabet}
\alias{Alphabet.as.character}
\alias{as.character,Alphabet-method}

 \title{Get the character representation of an Alphabet object}

 \description{
	Get the character representation of an Alphabet object.
 }

 \usage{
\method{as.character}{Alphabet}(x, ...)
}

 \arguments{
 	\item{x}{An Alphabet object}
 	\item{...}{Not used.}
 }

 \value{
 	Returns the character vector containing the symbol set.
 }

 \examples{

	# create alphabet object
	a<-Alphabet(symbols=c("A","T","G","C","N"))
	# get charcter representation
	as.character(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

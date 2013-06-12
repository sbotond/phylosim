%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSymbolLength.Alphabet}
\alias{getSymbolLength.Alphabet}
\alias{Alphabet.getSymbolLength}
\alias{getSymbolLength,Alphabet-method}

 \title{Get the length of the symbols in a given alphabet object}

 \description{
	Get the length of the symbols in a given alphabet object.
 }

 \usage{
\method{getSymbolLength}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{

	# create an alphabet object
	a<-Alphabet(symbols=c("AAA","AAC"));
	# get symbol length
	getSymbolLength(a);
	# get symbol length via virtual field
	a$symbolLength

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

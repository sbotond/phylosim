%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSymbols.Alphabet}
\alias{getSymbols.Alphabet}
\alias{Alphabet.getSymbols}
\alias{getSymbols,Alphabet-method}

 \title{Get the symbol set from an Alphabet object}

 \description{
	Get the symbol set from an Alphabet object.
 }

 \usage{
\method{getSymbols}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
	A character vector containing the symbol set of the Alphabet object.
 }

 \examples{

	# create a new alphabet object
	a<-Alphabet(symbols=c("AC","GT"));
	# get the symbols
	getSymbols(a)
	# get the symbols by using the virtual field
	a$symbols
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasSymbols.Alphabet}
\alias{hasSymbols.Alphabet}
\alias{Alphabet.hasSymbols}
\alias{hasSymbols,Alphabet-method}

 \title{Check if an Alphabet object has a given set of symbols}

 \description{
	Check if an Alphabet object has a given set of symbols.
	Returns true if the class of the object is "AnyAlphabet".
 }

 \usage{
\method{hasSymbols}{Alphabet}(this, sym, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{sym}{A character vector.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{

	# create alphabet object
	a<-Alphabet(symbols=c("A","T","G","C"));
	# check if it has the symbols "A" and "G"
	hasSymbols(a,c("A","G"))
	# check if has the symbol "X"
	hasSymbols(a,"X")
	# any alphabet returns true for every symbol
	b<-AnyAlphabet();
	hasSymbols(b,"X")

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

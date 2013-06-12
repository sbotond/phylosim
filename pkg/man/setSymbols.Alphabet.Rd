%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setSymbols.Alphabet}
\alias{setSymbols.Alphabet}
\alias{Alphabet.setSymbols}
\alias{setSymbols,Alphabet-method}

 \title{Specify a new symbol set for an Alphabet object}

 \description{
	Specify a new symbol set for an Alphabet object.
 }

 \usage{
\method{setSymbols}{Alphabet}(this, set, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{set}{The vector containing the new symbols set, automatically converted
	into a character vector.}
 	\item{...}{Not used.}
 }

 \value{
 	The new symbol set as a character vector.
 }

 \examples{

	# create a new alphabet object
	a<-Alphabet()
	a
	# specify a new symbol set
	setSymbols(a,c(0,1))
	a
	# the same, but now use the virtual field
	a$symbols<-c("A","T","G","C")
	a

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{isEmpty.Alphabet}
\alias{isEmpty.Alphabet}
\alias{Alphabet.isEmpty}
\alias{isEmpty,Alphabet-method}

 \title{Check if the symbol set is empty}

 \description{
	Check if the symbol set is empty.
 }

 \usage{
\method{isEmpty}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{

	# create an empty alphabet
	a<-Alphabet();
	# check whether it is empty
	isEmpty(a)
	# specify a new symbol set
	a$symbols<-c(0,1)
	isEmpty(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSize.Alphabet}
\alias{getSize.Alphabet}
\alias{Alphabet.getSize}
\alias{getSize,Alphabet-method}

 \title{Get the number of symbols in an Alphabet object}

 \description{
	Get the number of symbols in an Alphabet object.
 }

 \usage{
\method{getSize}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
	An integer vector of length one.
 }

 \examples{

	# create an alphabet object
	a<-Alphabet(symbols=c(0,1,2,3,4,5))
	a
	# get alphabet size
	getSize(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

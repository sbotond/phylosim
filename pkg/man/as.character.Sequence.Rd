%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.Sequence}
\alias{as.character.Sequence}
\alias{Sequence.as.character}
\alias{as.character,Sequence-method}

 \title{Get the string representation of a Sequence object}

 \description{
	Get the string representation of a Sequence object.
	The string representation is the concatenation of the states of the
	aggregated Site object. Undefined states (NA-s) are represented by question marks.
 }

 \usage{
\method{as.character}{Sequence}(x, ...)
}

 \arguments{
 	\item{x}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create object
	s<-Sequence(length=10)
	# get character representation
	as.character(s)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }

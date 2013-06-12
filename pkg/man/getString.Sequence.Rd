%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getString.Sequence}
\alias{getString.Sequence}
\alias{Sequence.getString}
\alias{getString,Sequence-method}

 \title{Get the string representation of a Sequence object}

 \description{
	Get the string representation of a Sequence object.
	The string representation is the concatenation of the states of the
	aggregated Site object. Undefined states (NA-s) are represented by question marks.
 }

 \usage{
\method{getString}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create object
	s<-Sequence(length=10)
	# get character representation
	getString(s)	# a bunch of '?'-s
	# get string reperesentation via virtual field
	s$string
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getId.Sequence}
\alias{getId.Sequence}
\alias{Sequence.getId}
\alias{getId,Sequence-method}

 \title{Get the unique identifier of a Sequence object}

 \description{
	Get the unique identifier of a Sequence object.
	The identifier is the concatenation of the object name and the object hash code as returned
	by hashCode().
 }

 \usage{
\method{getId}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A charcter vector of length one.
 }

 \examples{
	# create a Sequence object.
	s<-Sequence(length=5)
	# get id
	getId(s)
	# get id via virtual field
	s$id
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }

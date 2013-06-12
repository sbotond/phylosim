%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getLength.Sequence}
\alias{getLength.Sequence}
\alias{Sequence.getLength}
\alias{getLength,Sequence-method}

 \title{Get the number of Site objects aggregated in a Sequence object}

 \description{
	Get the number of Site objects aggregated in a Sequence object.
 }

 \usage{
\method{getLength}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	An integer vector of length one.
 }

 \examples{
	# create a Sequence object
	s<-Sequence(length=5)
	# get sequence length
	getLength(s)
	# get length via virtual field
	s$length
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }

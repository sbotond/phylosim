%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.QMatrix}
\alias{as.character.QMatrix}
\alias{QMatrix.as.character}
\alias{as.character,QMatrix-method}

 \title{Return the character representation of a QMatrix object}

 \description{
	Return the character representation of a QMatrix object.
 }

 \usage{
\method{as.character}{QMatrix}(x, ...)
}

 \arguments{
 	\item{x}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a QMatrix object
	m<-QMatrix()
	# get the character representation
	as.character(m)
	# the same, but implicitly
	m
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }

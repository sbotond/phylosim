%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{print.QMatrix}
\alias{print.QMatrix}
\alias{QMatrix.print}
\alias{print,QMatrix-method}

 \title{Print the character representation of a QMatrix object}

 \description{
	Print the character representation of a QMatrix object.
 }

 \usage{
\method{print}{QMatrix}(x, ...)
}

 \arguments{
 	\item{x}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	The character representation of the QMatrix object.
 }

 \examples{
	# create a QMatrix object
	m<-QMatrix()
	# print the character representation
	print(m)
	# the same, but implicitly
	m
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabet.QMatrix}
\alias{setAlphabet.QMatrix}
\alias{QMatrix.setAlphabet}
\alias{setAlphabet,QMatrix-method}

 \title{Set the Alphabet object for a QMatrix object}

 \description{
	Set the Alphabet object for a QMatrix object.

	This method rebuilds the scaled and unscaled rate matrices and so sets all rates to NA.
 }

 \usage{
\method{setAlphabet}{QMatrix}(this, value, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
	\item{value}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Alphabet object.
 }

 \examples{
	# create a QMatrix object
	m<-QMatrix()
	# set the alphabet
	setAlphabet(m,NucleotideAlphabet())
	# get the alphabet
	getAlphabet(m)
	# set alphabet via virtual field
	m$alphabet<-BinaryAlphabet()
	summary(m)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }

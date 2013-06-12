%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabet.QMatrix}
\alias{getAlphabet.QMatrix}
\alias{QMatrix.getAlphabet}
\alias{getAlphabet,QMatrix-method}

 \title{Get the Alphabet object associated with a QMatrix object}

 \description{
	Get the Alphabet object associated with a QMatrix object.
 }

 \usage{
\method{getAlphabet}{QMatrix}(this, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	An Alphabet object.
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

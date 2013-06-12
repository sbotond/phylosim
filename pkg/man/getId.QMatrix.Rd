%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getId.QMatrix}
\alias{getId.QMatrix}
\alias{QMatrix.getId}
\alias{getId,QMatrix-method}

 \title{Get the unique identifier of a QMatrix object}

 \description{
	Get the unique identifier of a QMatrix object.
 }

 \usage{
\method{getId}{QMatrix}(this, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a QMatrix object
	m<-QMatrix()
	# get object id
	getId(m)
	# get object id via virtual field
	m$id
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }

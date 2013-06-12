%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setName.QMatrix}
\alias{setName.QMatrix}
\alias{QMatrix.setName}
\alias{setName,QMatrix-method}

 \title{Set the name of a QMatrix object}

 \description{
	Set the name of a QMatrix object.
 }

 \usage{
\method{setName}{QMatrix}(this, value, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
	\item{value}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new object name.
 }

 \examples{
	# create a QMatrix object
	m<-QMatrix()
	# set/get name
	setName(m,"Susie Q")
	getName(m)
	# set/get name via virtual field
	m$name<-"Q"
	m$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }

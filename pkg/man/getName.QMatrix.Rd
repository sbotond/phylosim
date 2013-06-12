%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getName.QMatrix}
\alias{getName.QMatrix}
\alias{QMatrix.getName}
\alias{getName,QMatrix-method}

 \title{Get the name of a QMatrix object}

 \description{
	Get the name of a QMatrix object.
 }

 \usage{
\method{getName}{QMatrix}(this, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	A charcter vector of length one.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setLengthParam1.FastFieldDeletor}
\alias{setLengthParam1.FastFieldDeletor}
\alias{FastFieldDeletor.setLengthParam1}
\alias{setLengthParam1,FastFieldDeletor-method}

 \title{Set the first length parameter}

 \description{
	Set the first length parameter.
 }

 \usage{
\method{setLengthParam1}{FastFieldDeletor}(this, value, ...)
}

 \arguments{
 	\item{this}{A FastFieldDeletor object.}
 	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	value (invisible).
 }

 \examples{
	# create a geometric FastFieldDeletor
	p<-FastFieldDeletor()
	# set/get the first length parameter
	setLengthParam1(p,0.5)
	getLengthParam1(p)
	# set/get the first length parameter via virtual field
	p$lengthParam1<-0.2
	p$lengthParam1
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{FastFieldDeletor}}.
 }

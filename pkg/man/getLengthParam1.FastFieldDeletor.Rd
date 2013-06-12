%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getLengthParam1.FastFieldDeletor}
\alias{getLengthParam1.FastFieldDeletor}
\alias{FastFieldDeletor.getLengthParam1}
\alias{getLengthParam1,FastFieldDeletor-method}

 \title{Get the first length parameter}

 \description{
	Get the first length parameter.
 }

 \usage{
\method{getLengthParam1}{FastFieldDeletor}(this, ...)
}

 \arguments{
 	\item{this}{A FastFieldDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
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

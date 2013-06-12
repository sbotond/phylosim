%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setLengthParam2.FastFieldDeletor}
\alias{setLengthParam2.FastFieldDeletor}
\alias{FastFieldDeletor.setLengthParam2}
\alias{setLengthParam2,FastFieldDeletor-method}

 \title{Set the second length parameter}

 \description{
	Set the second length parameter.
 }

 \usage{
\method{setLengthParam2}{FastFieldDeletor}(this, value, ...)
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
	# set/get the second length parameter
	setLengthParam2(p,0.5)
	getLengthParam2(p)
	# set/get the second length parameter via virtual field
	p$lengthParam2<-0.2
	p$lengthParam2
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{FastFieldDeletor}}.
 }

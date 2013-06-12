%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setToleranceMargin.FastFieldDeletor}
\alias{setToleranceMargin.FastFieldDeletor}
\alias{FastFieldDeletor.setToleranceMargin}
\alias{setToleranceMargin,FastFieldDeletor-method}

 \title{Set the tolerance margin}

 \description{
	Set the tolerance margin.
 }

 \usage{
\method{setToleranceMargin}{FastFieldDeletor}(this, value, ...)
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
	# set/get tolerance margin
	setToleranceMargin(p,0.8)
	getToleranceMargin(p)
	# set/get tolerance margin via virtual field
	p$toleranceMargin<-0.75
	p$toleranceMargin
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{FastFieldDeletor}}.
 }

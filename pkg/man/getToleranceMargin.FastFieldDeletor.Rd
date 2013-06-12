%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getToleranceMargin.FastFieldDeletor}
\alias{getToleranceMargin.FastFieldDeletor}
\alias{FastFieldDeletor.getToleranceMargin}
\alias{getToleranceMargin,FastFieldDeletor-method}

 \title{Get the tolerance margin}

 \description{
	Get the tolerance margin.
 }

 \usage{
\method{getToleranceMargin}{FastFieldDeletor}(this, ...)
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

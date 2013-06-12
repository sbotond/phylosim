%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getLengthParam2.FastFieldDeletor}
\alias{getLengthParam2.FastFieldDeletor}
\alias{FastFieldDeletor.getLengthParam2}
\alias{getLengthParam2,FastFieldDeletor-method}

 \title{Get the second length parameter}

 \description{
	Get the second length parameter.
 }

 \usage{
\method{getLengthParam2}{FastFieldDeletor}(this, ...)
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

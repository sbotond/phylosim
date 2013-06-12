%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBaseFreqs.F81}
\alias{getBaseFreqs.F81}
\alias{F81.getBaseFreqs}
\alias{getBaseFreqs,F81-method}

 \title{Get the base frequency parameters}

 \description{
	Get the base frequency parameters.

 }

 \usage{
\method{getBaseFreqs}{F81}(this, ...)
}

 \arguments{
 	\item{this}{A F81 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the base frequency parameters.
 }

 \examples{
	# construct a F81 object
	p<-F81()
	# set/get base frequency parameters
	setBaseFreqs(p,c(2,1,2,1)/6)
	getBaseFreqs(p)
	# set/get base frequency parameters via virtual field
	p$baseFreqs<-c(4,4,1,1)/10
	p$baseFreqs
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{F81}}.
 }

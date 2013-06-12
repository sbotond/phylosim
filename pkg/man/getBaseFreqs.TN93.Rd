%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBaseFreqs.TN93}
\alias{getBaseFreqs.TN93}
\alias{TN93.getBaseFreqs}
\alias{getBaseFreqs,TN93-method}

 \title{Get the base frequency parameters}

 \description{
	Get the base frequency parameters.
 }

 \usage{
\method{getBaseFreqs}{TN93}(this, ...)
}

 \arguments{
 	\item{this}{A TN93 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the base frequencies.
 }

 \examples{
	# construct a TN93 object
	p<-TN93()
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
 	For more information see \code{\link{TN93}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTheta.T92}
\alias{getTheta.T92}
\alias{T92.getTheta}
\alias{getTheta,T92-method}

 \title{Get the GC content}

 \description{
       Get the GC content.

 }

 \usage{
\method{getTheta}{T92}(this, ...)
}

 \arguments{
       \item{this}{A T92 object.}
       \item{...}{Not used.}
 }

 \value{
       A numeric vector of length one.
 }

 \examples{
       # construct a T92 object
       p<-T92()
       # set/get GC content
	setTheta(p,0.6)
	getTheta(p)
	# set/get GC content via virtual field
	p$theta<-0.3
	p$theta
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{T92}}.
 }

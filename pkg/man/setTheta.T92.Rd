%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setTheta.T92}
\alias{setTheta.T92}
\alias{T92.setTheta}
\alias{setTheta,T92-method}

 \title{Set the GC content}

 \description{
       Set the GC content.

 }

 \usage{
\method{setTheta}{T92}(this, value, ...)
}

 \arguments{
       \item{this}{A T92 object.}
       \item{value}{A numeric vector of length one.}
       \item{...}{Not used.}
 }

 \value{
       The new value of theta (invisible).
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

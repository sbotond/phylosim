%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getScale.BrownianInsertor}
\alias{getScale.BrownianInsertor}
\alias{BrownianInsertor.getScale}
\alias{getScale,BrownianInsertor-method}

 \title{Get scale parameter}

 \description{
	Get scale parameter.
 }

 \usage{
\method{getScale}{BrownianInsertor}(this, ...)
}

 \arguments{
 	\item{this}{A BrownianInsertor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a BrownianInsertor object
	p<-BrownianInsertor(scale=0.002)
	# set/get scale parameter
	setScale(p,0.1)
	getScale(p)
	# set/get scale parameter via virtual field
	p$scale<-0.1
	p$scale
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{BrownianInsertor}}.
 }

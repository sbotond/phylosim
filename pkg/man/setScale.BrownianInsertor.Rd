%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setScale.BrownianInsertor}
\alias{setScale.BrownianInsertor}
\alias{BrownianInsertor.setScale}
\alias{setScale,BrownianInsertor-method}

 \title{Set scale parameter}

 \description{
	Set scale parameter.
 }

 \usage{
\method{setScale}{BrownianInsertor}(this, value, ...)
}

 \arguments{
 	\item{this}{A BrownianInsertor object.}
 	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	value (invisible).
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

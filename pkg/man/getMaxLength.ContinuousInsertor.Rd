%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMaxLength.ContinuousInsertor}
\alias{getMaxLength.ContinuousInsertor}
\alias{ContinuousInsertor.getMaxLength}
\alias{getMaxLength,ContinuousInsertor-method}

 \title{Get the maximum length}

 \description{
	Get the maximum length.
 }

 \usage{
\method{getMaxLength}{ContinuousInsertor}(this, ...)
}

 \arguments{
 	\item{this}{A ContinuousInsertor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create object
	o<-ContinuousInsertor(rate=1)
	# set length sampling expression via virtual field
	o$dist<-expression(rnorm(1,mean=6,sd=3))
	# set/get maxLength
	setMaxLength(o, 3)
	getMaxLength(o)
	# set/get maxLength via virtual field
	o$maxLength<-10
	o$maxLength
	# propose a length
	proposeLength(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ContinuousInsertor}}.
 }

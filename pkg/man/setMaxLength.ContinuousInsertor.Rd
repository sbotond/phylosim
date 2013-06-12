%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setMaxLength.ContinuousInsertor}
\alias{setMaxLength.ContinuousInsertor}
\alias{ContinuousInsertor.setMaxLength}
\alias{setMaxLength,ContinuousInsertor-method}

 \title{Set the maximum length}

 \description{
	Set the maximum length.
 }

 \usage{
\method{setMaxLength}{ContinuousInsertor}(this, value, ...)
}

 \arguments{
 	\item{this}{A ContinuousInsertor object.}
	\item{value}{A numeric (integer) vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new maximum length.
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

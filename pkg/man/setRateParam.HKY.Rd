%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.HKY}
\alias{setRateParam.HKY}
\alias{HKY.setRateParam}
\alias{setRateParam,HKY-method}

 \title{Set the value of a rate parameter}

 \description{
	Set the value of a rate parameter.

	 The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{setRateParam}{HKY}(this, name, value, ...)
}

 \arguments{
 	\item{this}{An HKY object.}
	\item{name}{The name of the rate parameter.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate parameter (invisible).
 }

 \examples{
	# construct HKY object
	p<-HKY();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{HKY}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.TN93}
\alias{setRateParam.TN93}
\alias{TN93.setRateParam}
\alias{setRateParam,TN93-method}

 \title{Set the value of a rate parameter}

 \description{
	Set the value of a rate parameter.

	 The rate parameters are: Alpha1, Alpha2, Beta.
 }

 \usage{
\method{setRateParam}{TN93}(this, name, value, ...)
}

 \arguments{
 	\item{this}{A TN93 object.}
	\item{name}{The name of the rate parameter.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate parameter (invisible).
 }

 \examples{
	# construct a TN93 object
	p<-TN93();
	# set/get a rate parameter
	setRateParam(p,"Beta",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{TN93}}.
 }

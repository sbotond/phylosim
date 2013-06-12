%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.K80}
\alias{setRateParam.K80}
\alias{K80.setRateParam}
\alias{setRateParam,K80-method}

 \title{Set the value of a rate parameter}

 \description{
	Set the value of a rate parameter.

	 The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{setRateParam}{K80}(this, name, value, ...)
}

 \arguments{
 	\item{this}{A K80 object.}
	\item{name}{The name of the rate parameter.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate parameter (invisible).
 }

 \examples{
	# construct a K80 object
	p<-K80();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K80}}.
 }

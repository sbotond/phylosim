%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.K81}
\alias{setRateParam.K81}
\alias{K81.setRateParam}
\alias{setRateParam,K81-method}

 \title{Set the value of a rate parameter}

 \description{
	Set the value of a rate parameter.

	 The rate parameters are: Alpha, Beta, Gamma.
 }

 \usage{
\method{setRateParam}{K81}(this, name, value, ...)
}

 \arguments{
 	\item{this}{A K81 object.}
	\item{name}{The name of the rate parameter.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate parameter (invisible).
 }

 \examples{
	# construct a K80 object
	p<-K81();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Gamma")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K81}}.
 }

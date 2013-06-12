%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.GTR}
\alias{setRateParam.GTR}
\alias{GTR.setRateParam}
\alias{setRateParam,GTR-method}

 \title{Set the value of a rate parameter}

 \description{
	Set the value of a rate parameter.

	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
 }

 \usage{
\method{setRateParam}{GTR}(this, name, value, ...)
}

 \arguments{
 	\item{this}{A GTR object.}
	\item{name}{The name of the rate parameter.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate parameter (invisible).
 }

 \examples{
	# construct a GTR object
	p<-GTR();
	# set/get a rate parameter
	setRateParam(p,"a",4)
	getRateParam(p,"a")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GTR}}.
 }

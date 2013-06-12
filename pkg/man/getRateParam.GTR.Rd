%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.GTR}
\alias{getRateParam.GTR}
\alias{GTR.getRateParam}
\alias{getRateParam,GTR-method}

 \title{Get the value of a rate parameter}

 \description{
	Get the value of a rate parameter.

	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
 }

 \usage{
\method{getRateParam}{GTR}(this, name, ...)
}

 \arguments{
 	\item{this}{A GTR object.}
	\item{name}{The name of the rate parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
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
